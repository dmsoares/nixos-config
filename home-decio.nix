{ lib, config, pkgs, pkgs-22_11, ... }:

{
	home = {
		username = "decio";
		homeDirectory = "/home/decio";
		packages = with pkgs; [
		      	discord
		      	google-chrome
		      	vscode

		      	# utils
		      	fd
		      	fzf
		      	jq
		      	ripgrep

			# dev
			nodejs
		   	nodePackages.npm
	    		pkgs-22_11.nodePackages.pnpm #pnpm v7

			libgcc

			ghc
		];

		sessionVariables = {
		      	BROWSER = "google-chrome-stable";
		      	EDITOR = "nvim";
			TERMINAL = "alacritty";
		};

		stateVersion = "23.11";
	};

	imports = [
		./xmonad
		./rofi
	];

	programs = {
		home-manager.enable = true;

		git = {
			enable = true;
			userName = "Decio Soares";
			userEmail = "decio.msoares@gmail.com";
		};

		alacritty = {
			enable = true;
			# custom settings
			settings = {
				env.TERM = "xterm-256color";
				font = {
					size = 12;
					draw_bold_text_with_bright_colors = true;
				};
				scrolling.multiplier = 5;
				selection.save_to_clipboard = true;
			};
		};

		vscode = {
			enable = true;
			extensions = with pkgs.vscode-extensions; [
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
	};
}
