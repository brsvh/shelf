{
  config,
  my,
  pkgs,
  ...
}:
let
  inherit (pkgs)
    writeText
    ;

  monitors = writeText "monitors.xml" ''
    <monitors version="2">
      <configuration>
        <layoutmode>logical</layoutmode>
        <logicalmonitor>
          <x>0</x>
          <y>0</y>
          <scale>1.25</scale>
          <primary>yes</primary>
          <monitor>
            <monitorspec>
              <connector>eDP-1</connector>
              <vendor>CMN</vendor>
              <product>0x1301</product>
              <serial>0x00000000</serial>
            </monitorspec>
            <mode>
              <width>2160</width>
              <height>1350</height>
              <rate>59.744</rate>
            </mode>
          </monitor>
        </logicalmonitor>
      </configuration>
    </monitors>
  '';
in
{
  imports = [
    my.diskoConfigurations.eustoma
    my.nixosProfiles.binfmt
    my.nixosProfiles.chinese
    my.nixosProfiles.dconf
    my.nixosProfiles.docker
    my.nixosProfiles.fprintd
    # my.nixosProfiles.guix
    my.nixosProfiles.libvirt
    my.nixosProfiles.virtual-camera
    my.nixosProfiles.secure-boot
    # REVIEW remove this file after nixos-facter support setup Touch Pad
    # See https://github.com/numtide/nixos-facter-modules/issues/47 .
    my.nixosProfiles.touchpad
    my.nixosSecrets.eustoma
    my.nixosSuites.gnomeWorkstation
    my.nixosUsers.bsc
  ];

  console = {
    keyMap = "us";
  };

  facter = {
    reportPath = my.etc.facter.eustoma;
  };

  home-manager = {
    sharedModules = [
      {
        xdg = {
          configFile = {
            "monitors.xml" = {
              source = monitors;
            };
          };
        };
      }
    ];
  };

  i18n = {
    languages = {
      english = {
        isDefault = true;
      };
    };
  };

  networking = {
    hostName = "eustoma";
  };

  system = {
    stateVersion = "25.05";
  };

  systemd = {
    tmpfiles = {
      rules =
        let
          inherit (config.users.users.gdm)
            home
            ;
        in
        [
          "d ${home}/.config 0711 gdm gdm"
          "L+ ${home}/.config/monitors.xml - - - - ${monitors}"
        ];
    };
  };

  time = {
    timeZone = "Asia/Shanghai";
  };

  virtualisation = {
    docker = {
      # TODO check storageDriver by current filesystems.
      storageDriver = "btrfs";
    };
  };
}
