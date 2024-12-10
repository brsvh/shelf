{
  lib,
  stdenv,
  fetchurl,
  dpkg,
  autoPatchelfHook,
  makeWrapper,
  nss,
  xorg,
  desktop-file-utils,
  libpulseaudio,
  libgcrypt,
  dbus,
  systemd,
  udev,
  libGL,
  fontconfig,
  freetype,
  openssl,
  wayland,
  libdrm,
  harfbuzz,
  openldap,
  curl,
  nghttp2,
  libunwind,
  alsa-lib,
  libidn2,
  rtmpdump,
  libpsl,
  libkrb5,
  xkeyboard_config,
  libsForQt5,
  gcc,
  pkg-config,
  fetchFromGitHub,
  cmake,
  ninja,
  wireplumber,
  libportal,
  xdg-desktop-portal,
  opencv,
  pipewire,
  fetchgit,
}:
let
  wemeet-wayland-screenshare = stdenv.mkDerivation {
    pname = "wemeet-wayland-screenshare";
    version = "0-unstable-2024-12-07";

    src = fetchFromGitHub {
      owner = "xuwd1";
      repo = "wemeet-wayland-screenshare";
      rev = "d2f0f3b3ac0dce2c890d68c38e8f253ea7ab23ca";
      hash = "sha256-HptibtvhRVKQZe0fjy315QU2iM59B/glPH+0t/tM/ns=";
      fetchSubmodules = true;
    };

    nativeBuildInputs = [
      cmake
      ninja
      pkg-config
    ];

    buildInputs = [
      wireplumber
      libportal
      xdg-desktop-portal
      libsForQt5.qtwayland
      libsForQt5.xwaylandvideobridge
      opencv
      pipewire
      xorg.libXdamage
      xorg.libXrandr
      xorg.libX11
    ];

    dontWrapQtApps = true;

    meta = {
      description = "Hooked WeMeet that enables screenshare on Wayland";
      homepage = "https://github.com/xuwd1/wemeet-wayland-screenshare";
      license = lib.licenses.mit;
      maintainers = with lib.maintainers; [ aucub ];
    };
  };
  libwemeetwrap = stdenv.mkDerivation {
    pname = "libwemeetwrap";
    version = "1.0";

    src = fetchgit {
      url = "https://aur.archlinux.org/wemeet-bin.git";
      rev = "8f03fbc4d5ae263ed7e670473886cfa1c146aecc";
      hash = "sha256-ExzLCIoLu4KxaoeWNhMXixdlDTIwuPiYZkO+XVK8X10=";
    };

    dontWrapQtApps = true;

    nativeBuildInputs = [
      gcc
      pkg-config
    ];

    buildInputs = [
      openssl
      libpulseaudio
      xorg.libX11
    ];

    buildPhase = ''
      runHook preBuild

      read -ra openssl_args < <(pkg-config --libs openssl)
      read -ra libpulse_args < <(pkg-config --cflags --libs libpulse)
      # Comment out `-D WRAP_FORCE_SINK_HARDWARE` to disable the patch that forces wemeet detects sink as hardware sink
      gcc $CFLAGS -Wall -Wextra -fPIC -shared \
        "''${openssl_args[@]}" "''${libpulse_args[@]}" \
        -o libwemeetwrap.so wrap.c -D WRAP_FORCE_SINK_HARDWARE

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      install -Dm755 ./libwemeetwrap.so $out/lib/libwemeetwrap.so

      runHook postInstall
    '';

    meta = {
      license = lib.licenses.unfree;
      maintainers = with lib.maintainers; [ aucub ];
    };
  };
  selectSystem =
    attrs:
    attrs.${stdenv.hostPlatform.system}
      or (throw "wemeet: ${stdenv.hostPlatform.system} is not supported");
in
stdenv.mkDerivation {
  pname = "wemeet";
  version = "3.19.2.400";

  src = selectSystem {
    x86_64-linux = fetchurl {
      url = "https://updatecdn.meeting.qq.com/cos/fb7464ffb18b94a06868265bed984007/TencentMeeting_0300000000_3.19.2.400_x86_64_default.publish.officialwebsite.deb";
      hash = "sha256-PSGc4urZnoBxtk1cwwz/oeXMwnI02Mv1pN2e9eEf5kE=";
    };
    aarch64-linux = fetchurl {
      url = "https://updatecdn.meeting.qq.com/cos/867a8a2e99a215dcd4f60fe049dbe6cf/TencentMeeting_0300000000_3.19.2.400_arm64_default.publish.officialwebsite.deb";
      hash = "sha256-avN+PHKKC58lMC5wd0yVLD0Ct7sbb4BtXjovish0ULU=";
    };
  };

  nativeBuildInputs = [
    dpkg
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = [
    nss
    xorg.libX11
    desktop-file-utils
    libpulseaudio
    libgcrypt
    dbus
    systemd
    udev
    libGL
    xorg.libSM
    xorg.libICE
    xorg.libXtst
    libGL
    fontconfig
    freetype
    openssl
    wayland
    libdrm
    harfbuzz
    openldap
    curl
    nghttp2
    libunwind
    alsa-lib
    libidn2
    rtmpdump
    libpsl
    libkrb5
    xkeyboard_config
  ];

  installPhase = ''
    runHook preInstall

    cp -r opt/wemeet $out
    cp -r usr/* $out
    rm $out/lib/libcurl.so
    substituteInPlace $out/share/applications/wemeetapp.desktop \
      --replace-fail "/opt/wemeet/wemeetapp.sh" "wemeetapp" \
      --replace-fail "/opt/wemeet/wemeet.svg" "wemeet"
    sed -i "s|^Prefix.*|Prefix = $out/lib|" $out/bin/qt.conf
    mv $out/icons $out/share/icons
    install -Dm0644 $out/wemeet.svg $out/share/icons/hicolor/scalable/apps/wemeet.svg
    ln -s $out/bin/raw/xcast.conf $out/bin/xcast.conf
    ln -s $out/plugins $out/lib/plugins
    ln -s $out/resources $out/lib/resources
    mkdir -p $out/lib/translations
    ln -s $out/translations/qtwebengine_locales $out/lib/translations/qtwebengine_locales

    runHook postInstall
  '';

  preFixup = ''
    wrapProgram $out/bin/wemeetapp \
      --set XDG_SESSION_TYPE "x11" \
      --set EGL_PLATFORM "x11" \
      --set LP_NUM_THREADS "2" \
      --set QT_QPA_PLATFORM "xcb" \
      --set QT_STYLE_OVERRIDE "fusion" \
      --set QT_AUTO_SCREEN_SCALE_FACTOR "1" \
      --set IBUS_USE_PORTAL "1" \
      --unset WAYLAND_DISPLAY \
      --set TZ "Asia/Shanghai" \
      --set XKB_CONFIG_ROOT "${xkeyboard_config}/share/X11/xkb" \
      --set LC_ALL "zh_CN.UTF-8" \
      --prefix LD_LIBRARY_PATH : "$out/x11-wayland/1050/lib/${
        selectSystem {
          x86_64-linux = "x86_64-linux-gnu";
          aarch64-linux = "aarch64-linux-gnu";
        }
      }:$out/lib:$out/plugins:$out/resources:$out/translations:${xorg.libXext}/lib:${xorg.libXdamage}/lib:${opencv}/lib:${xorg.libXrandr}/lib" \
      --prefix PATH : "$out/bin" \
      --prefix QT_PLUGIN_PATH : "$out/plugins" \
      --run "mkdir -p \$HOME/.local/share/wemeetapp" \
      --prefix LD_PRELOAD : "${libwemeetwrap}/lib/libwemeetwrap.so:${wemeet-wayland-screenshare}/lib/wemeet/libhook.so" \
      --set USER_RUN_DIR "/run/user/$(id -u)" \
      --set FONTCONFIG_DIR "$CONFIG_DIR/fontconfig" \
      --set KDE_GLOBALS_FILE "$CONFIG_DIR/kdeglobals" \
      --set LD_PRELOAD_WRAP "$LD_PRELOAD:${libwemeetwrap}/lib/libwemeetwrap.so" \
      --set CONFIG_DIR "''${XDG_CONFIG_HOME:-$HOME/.config}" \
      --set KDE_ICON_CACHE_FILE "''${XDG_CACHE_HOME:-$HOME/.cache}/icon-cache.kcache" \
      --set WEMEET_APP_DIR "''${XDG_DATA_HOME:-$HOME/.local/share}/wemeetapp"
  '';

  meta = {
    description = "Tencent Video Conferencing";
    homepage = "https://wemeet.qq.com";
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    license = lib.licenses.unfree;
    maintainers = with lib.maintainers; [ aucub ];
    mainProgram = "wemeetapp";
  };
}
