{
  disko = {
    devices = {
      disk = {
        nvme0n1 = {
          content = {
            partitions = {
              efi = {
                content = {
                  format = "vfat";
                  mountpoint = "/boot/efi";
                  type = "filesystem";
                };

                name = "efi";
                priority = 1;
                size = "1G";
                type = "EF00";
              };

              boot = {
                content = {
                  format = "ext4";
                  mountpoint = "/boot";
                  type = "filesystem";
                };

                name = "boot";
                priority = 2;
                size = "1G";
                type = "EF02";
              };

              linux = {
                content = {
                  extraArgs = [ "-f" ];

                  subvolumes = {
                    "/gnu" = {
                      mountOptions = [
                        "compress=zstd"
                        "noatime"
                        "ssd"
                      ];

                      mountpoint = "/gnu";
                      name = "gnu";
                    };

                    "/home" = {
                      mountOptions = [
                        "compress=zstd:1"
                        "ssd"
                      ];

                      mountpoint = "/home";
                      name = "home";
                    };

                    "/nix" = {
                      mountOptions = [
                        "compress=zstd:1"
                        "noatime"
                        "ssd"
                      ];

                      mountpoint = "/nix";
                      name = "nix";
                    };

                    "/nixos" = {
                      mountOptions = [
                        "compress=zstd:1"
                        "ssd"
                      ];

                      mountpoint = "/";
                      name = "nixos";
                    };

                    "/src" = {
                      mountOptions = [
                        "compress=zstd:1"
                        "ssd"
                      ];

                      mountpoint = "/src";
                      name = "src";
                    };
                  };

                  type = "btrfs";
                };

                name = "linux";
                size = "100%";
              };
            };

            type = "gpt";
          };

          device = "/dev/nvme0n1";
          type = "disk";
        };
      };
    };
  };
}
