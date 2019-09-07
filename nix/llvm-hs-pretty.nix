let
  url = "https://github.com/llvm-hs/llvm-hs-pretty";
  ref = "master";
  rev = "b88af27f194571ade68c6fa89f9d3e662246e0a8";
  repo = builtins.fetchGit { inherit url ref rev; };
in
  repo
