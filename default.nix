(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    back          = ./back;
    common        = ./common;
    front         = ./front;
    #esr-front-warp    = ./front-warp;
    #esr-front-android = ./front-android;
    #esr-front-ghcjs   = ./front-ghcjs;
  };

  withHoogle = false;

  shells = {
    ghc   = ["front" "common" "back"];
    ghcjs = ["front" "common" ];
  };
})
