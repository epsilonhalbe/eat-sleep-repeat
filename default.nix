(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    back          = ./back;
    front         = ./front;
    #esr-front-warp    = ./front-warp;
    #esr-front-android = ./front-android;
    #esr-front-ghcjs   = ./front-ghcjs;
  };

  withHoogle = false;

  shells = {
    ghc   = ["front" "back"];
    ghcjs = ["front" ];
  };
})
