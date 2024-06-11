{ config, ... }:
{
  home = {
    sessionVariables = {
      NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/npmrc";
    };
  };

  xdg = {
    configFile = {
      "npm/npmrc".text = ''
        prefix=''${XDG_DATA_HOME}/npm
        cache=''${XDG_CACHE_HOME}/npm
        init-module=''${XDG_CONFIG_HOME}/npm/config/npm-init.js
        logs-dir=''${XDG_STATE_HOME}/npm/logs
      '';
    };
  };
}
