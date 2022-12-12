{ pkgs ? import <nixpkgs> { }
, system ? builtins.currentSystem
, devshell ? import (fetchTarball "https://github.com/numtide/devshell/archive/master.tar.gz") { inherit system; }
}:
let
  inherit (pkgs)
    writeShellApplication
    pandoc
    jq
    curl
    ;

  # Writes my-file to /nix/store/<store path>/bin/my-file and makes executable.
  update-readme = writeShellApplication {
    name = "update-readme";
    runtimeInputs = [ jq curl pandoc ];
    text = ''
      # Generate a personal access token and export it as PRIVATE_BEARER_TOKEN:
      [ -e /tmp/dotfield-readme-update-access-token.txt ] && bearer_token="$(cat /tmp/dotfield-readme-update-access-token.txt)"

      pandoc README.org -f org -t gfm -o /tmp/README.md --table-of-contents -s
      pandoc /tmp/README.md -f gfm -t html5 -o /tmp/README.html

      repo_id=234254

      # And the readme file:
      readme=/tmp/README.html

      # Set the repository's README
      jq -sR '{
          "query": "mutation UpdateRepo($id: Int!, $readme: String!) {
            updateRepository(id: $id, input: { readme: $readme }) { id }
          }", "variables": {
            "id": '$repo_id',
            "readme": .
          } }' < $readme \
        | curl --oauth2-bearer "$bearer_token" \
          -H "Content-Type: application/json" \
          -d@- https://git.sr.ht/query

      echo
      echo "Updated project README with $(wc -l < "$readme") lines"
    '';
  };
in
devshell.mkShell {
  commands = [
    {
      name = "cachix-develop";
      category = "utils";
      command = "cachix watch-exec virtual-dts-mode nix -- develop";
      help = "Build devshell and push to cachix";
    }
    {
      name = "update-readme";
      category = "utils";
      package = update-readme;
      help = "Convert the README.org to HTML and update the sourcehut README";
    }
  ];
}
