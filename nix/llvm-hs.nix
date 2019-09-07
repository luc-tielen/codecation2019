let
  url = "https://github.com/llvm-hs/llvm-hs";
  ref = "llvm-7";
  rev = "3b4e35e1abd88f8f30400242503d7cb25f9be253";
  repo = builtins.fetchGit { inherit url ref rev; };
in
  repo
