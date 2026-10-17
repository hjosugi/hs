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
            fileset = pkgs.lib.fileset.unions [ ./Must50DCore.hs ./Common ./Solutions ./app ./test ];
          };
          runner = name: command: entry: pkgs.writeShellApplication {
            inherit name;
            runtimeInputs = [ ghc ];
            text = ''
              exec ${command} -XHaskell2010 -i"${source}" "${source}/${entry}" "$@"
            '';
          };
          demo = runner "must50-hs" "runghc" "app/Main.hs";
          test = runner "must50-test" "runghc" "test/Check.hs";
          repl = runner "must50-repl" "ghci" "Must50DCore.hs";
          problemModules = {
            p61 = "MergeIntervals";
            p64 = "MeetingRoomsII";
            p68 = "NumberOfIslands";
            p75 = "CourseSchedule";
            p80 = "WordLadder";
            p82 = "NetworkDelayTime";
            p34 = "LRUCache";
            p5 = "TopKFrequentElements";
            p52 = "MedianFinder";
            p14 = "LongestSubstringWithoutRepeatingCharacters";
            p108 = "ImplementTrie";
            p111 = "WordSearchII";
            p101 = "SlidingWindowMaximum";
            p93 = "WordBreak";
            p86 = "HouseRobber";
            p95 = "PartitionEqualSubsetSum";
            p94 = "LongestIncreasingSubsequence";
            p45 = "BinaryTreeMaximumPathSum";
            p21 = "DailyTemperatures";
            p54 = "CombinationSum";
            p25 = "KokoEatingBananas";
            p79 = "RedundantConnection";
            p267 = "SubarraySumEqualsK";
            p268 = "IncrementSubmatrices";
            p270 = "MinimumCostToCutOffAllLeaves";
            p260 = "StatusMessageDeduplication";
          };
          problemPackages = pkgs.lib.mapAttrs
            (name: module: runner "must50-${name}" "runghc" "Solutions/${module}.hs")
            problemModules;
        in {
          packages = { default = demo; inherit test repl; } // problemPackages;
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
          } // pkgs.lib.mapAttrs (name: package: {
            type = "app";
            program = "${package}/bin/must50-${name}";
            meta.description = "Run the checked sample for problem ${name}";
          }) problemPackages;
          devShell = pkgs.mkShell {
            packages = [
              ghc
              pkgs.cabal-install
              pkgs.haskellPackages.haskell-language-server
              pkgs.haskellPackages.fourmolu
            ];
          };
          check = pkgs.runCommand "must50-hs-check" {
            nativeBuildInputs = [ ghc ];
          } ''
            ghc -XHaskell2010 -Wall -Werror -fno-code -i${source} ${source}/app/Main.hs
            ghc -XHaskell2010 -Wall -Werror -fno-code -i${source} ${source}/test/Check.hs
            runghc -XHaskell2010 -i${source} ${source}/test/Check.hs
            for solution in ${source}/Solutions/*.hs; do
              runghc -XHaskell2010 -i${source} "$solution"
            done
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
