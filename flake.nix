{
  description = "Must 50 Haskell examples and development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/319e4cebef10cbb636c6ea392137b52ad105336a";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      environment = system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ghc = pkgs.haskellPackages.ghcWithPackages (ps: [ ps.hashtables ]);
          source = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [ ./Must50DCore.hs ./app ./test ];
          };
          runner = name: command: entry: pkgs.writeShellApplication {
            inherit name;
            runtimeInputs = [ ghc ];
            text = ''
              exec ${command} -i"${source}" "${source}/${entry}" "$@"
            '';
          };
          demo = runner "must50-hs" "runghc" "app/Main.hs";
          test = runner "must50-test" "runghc" "test/Check.hs";
          repl = runner "must50-repl" "ghci" "Must50DCore.hs";
        in {
          packages = { default = demo; inherit test repl; };
          apps = {
            default = {
              type = "app";
              program = "${demo}/bin/must50-hs";
              meta.description = "Run the Haskell examples";
            };
            test = {
              type = "app";
              program = "${test}/bin/must50-test";
              meta.description = "Test all 25 problems, bonus 260, and follow-ups";
            };
            repl = {
              type = "app";
              program = "${repl}/bin/must50-repl";
              meta.description = "Open GHCi with all solutions loaded";
            };
          };
          devShell = pkgs.mkShell {
            packages = [ ghc pkgs.cabal-install ];
          };
          check = pkgs.runCommand "must50-hs-check" {
            nativeBuildInputs = [ ghc ];
          } ''
            ghc -Wall -Werror -fno-code -i${source} ${source}/app/Main.hs
            ghc -Wall -Werror -fno-code -i${source} ${source}/test/Check.hs
            runghc -i${source} ${source}/test/Check.hs
            touch "$out"
          '';
        };
    in {
      packages = forAllSystems (system: (environment system).packages);
      apps = forAllSystems (system: (environment system).apps);
      devShells = forAllSystems (system: { default = (environment system).devShell; });
      checks = forAllSystems (system: { default = (environment system).check; });
    };
}
