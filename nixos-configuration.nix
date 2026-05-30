# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }: with builtins;

let
  # There's no XPS 13 9315 entry, so we cobble together a few
  # pieces from here.
  nixos_hardware =
    builtins.fetchGit {
      url = "https://github.com/NixOS/nixos-hardware.git";
      ref = "master";
      rev = "2096f3f411ce46e88a79ae4eafcfc9df8ed41c61";
    };

in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      "${nixos_hardware}/common/cpu/intel"
      "${nixos_hardware}/common/pc/laptop"
      "${nixos_hardware}/common/pc/ssd"
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Our root swap partitions virtual are, managed by a single encrypted LVM
  # partition.  This tells NixOS that we need to decrypt the partition on boot.
  # Encrypted setup comes from this awesome guide:
  # https://qfpl.io/posts/installing-nixos/
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };

  # In 6.18 the webcam looks awful, but it does work
  # Copy fail and related patch
  boot.kernelPackages = lib.mkIf (lib.versionOlder pkgs.linux.version "6.18.31") (
    lib.mkDefault pkgs.linuxPackages_6_18
  );

  # Ensure that closing the lid of the laptop puts it to sleep (XPS 13 specific)
  boot.kernelParams = [ "mem_sleep_default=deep" ];

  # This seems to be highly recommended for most XPS models
  services.thermald.enable = lib.mkDefault true;

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Use nmcli to manage networks or connecting to WiFi

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  console.keyMap = "dvorak";
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    autorun = true;
    # layout = "dvorak";
    layout = "us";
    xkbVariant = "dvorak";

    # Touchpad options
    libinput = {
      enable = true; # Enable the touchpad
      touchpad.tapping = false; # disable tap-to-click
    };

    # disable the default of just loading into a single xterm terminal emulator
    desktopManager.xterm.enable = false;

    # Our entrypoint for login
    displayManager.lightdm.enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  # Needed for webcam
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = [ "gtk" ];
  };

  # Get some nice transparency out of inactive windows using
  # the picom compositor (not something that XMonad or X11
  # provide themselves).
  services.picom = {
    enable = true;
    inactiveOpacity = 0.8;
  };

  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  services.pipewire.enable = true;

  # Fix internal microphone on XPS 13, suspend permanently drops the hardware gain
  # TODO: Occasionally the mic just seems to totally die after suspend, but another suspend+resume seems to fix it.
  powerManagement.powerUpCommands = ''
    ${pkgs.alsa-utils}/bin/amixer -c sofsoundwire cset name='rt714 FU0C Boost' 3
    ${pkgs.alsa-utils}/bin/amixer -c sofsoundwire cset name='rt714 FU0E Boost' 3
  '';

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.nathan = {
    createHome = true;
    group = "users";
    home = "/home/nathan";
    isNormalUser = true;
    extraGroups = [
      # "wheel" enables ‘sudo’ for the user.
      "wheel"
      "video"
      "audio"
      "disk"
      "networkmanager"
      # Full access to serial ports, especially useful for hardware development (microcontroller/fpga/etc)
      "dialout"
    ];
    packages = with pkgs; [
      chromium
      zoom-us
    ];
    uid = 1000;
  };

  programs.firefox = {
    enable = true;
    preferences = {
      # For XPS 13 webcam
      "media.webrtc.camera.allow-pipewire" = true;
    };
  };

  environment.variables.EDITOR = "vim";
  environment.variables.SUDO_EDITOR = "vim";
  environment.variables.BROWSER = "chromium";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    ((vim.override {}).customize {
      vimrcConfig = {
        packages.myVimPackage = with vimPlugins; {
          start = [
            # TODO: More plugins
            vim-nix
            lean-nvim
          ];
        };
        customRC = builtins.readFile ./.vimrc;
      };
    })
    wget
    git
    networkmanagerapplet
    nix-prefetch-scripts
    # nix-repl // TODO: Not maintained, but I'm not sure exactly what the alternative is.
    which
    xmobar
    alacritty
    rofi
    cabal2nix
    # Ideally, we want to figure out how to do a full nix mode
    stack
  ];

  # By default, NixOS only allows freely licensed software.  Rather than
  # disabling this, we can allow specific packages.
  nixpkgs.config.allowUnfreePredicate =
    pkg: (lib.getName pkg) == "zoom" || (lib.getName pkg) == "ipu6-camera-bin" || (lib.getName pkg) == "ipu6ep-camera-bin" || (lib.getName pkg) == "ivsc-firmware";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # Vi keybindings in bash, ftw!
  programs.bash.interactiveShellInit = "set -o vi";

  programs.git = {
    enable = true;
    config = {
      # user.name = "";
      # user.email = "";

      # Show the common ancestor (what the two conflicting sides originally
      # saw) when resolving merge conflicts.  This can help a lot in
      # understanding the context of both changes and why they conflict.
      merge.conflictstyle = "diff3";

      # Universally ignore files that vim uses
      core.excludesFile = [ "*.swp" "*.swo" ];
    };
  };

  # List services that you want to enable:

  # Use physlock to manage screen locking.  It's pretty ugly,
  # but it works.
  services.physlock.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
