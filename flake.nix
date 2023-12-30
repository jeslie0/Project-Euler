{
  description = "My Project Euler solutions, in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        packageName = "project-euler";
      in
        {

          packages = {
            default = self.packages.${system}.${packageName};
            ${packageName} = haskellPackages.callCabal2nix packageName self {};
          };

          devShell = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.default ]; # This automatically pulls cabal libraries into the devshell, so they can be used in ghci
            buildInputs = with haskellPackages; [ haskell-language-server
                                                  cabal-install
                                                ];

            # This will build the cabal project and add it to the path. We probably don't want that to happen.
            # inputsFrom = builtins.attrValues self.packages.${system};

            # Enables Hoogle for the builtin packages.
            withHoogle = true;
          };
        }
    );
}
