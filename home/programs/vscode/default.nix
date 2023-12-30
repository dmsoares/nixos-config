{ pkgs-unstable, ... }:
{
  programs.vscode = {
    enable = true;

    package = pkgs-unstable.vscode;

    extensions = with pkgs-unstable.vscode-extensions; [
      esbenp.prettier-vscode
      dbaeumer.vscode-eslint
      bradlc.vscode-tailwindcss
      ms-vscode.cpptools
      github.copilot
      bbenoist.nix
      hashicorp.terraform
      yzhang.markdown-all-in-one
    ];
  };
}
