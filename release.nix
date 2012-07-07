/* Nix build recipe for Skribilo.
   Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>

   This file is part of Skribilo.

   Skribilo is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Skribilo is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Skribilo.  If not, see <http://www.gnu.org/licenses/>.  */

let
  buildOutOfSourceTree = true;

  jobs = {
    tarball =
      let
	pkgs = import <nixpkgs> {};
        greader = (import <guile-reader/release.nix>).build {};
      in
	pkgs.releaseTools.sourceTarball {
	  name = "skribilo-tarball";
	  src = <skribilo>;
          buildNativeInputs = [ pkgs.git ];       # for `git-version-gen'
	  buildInputs = [ greader ]
            ++ (with pkgs; [ gettext texinfo guile guile_lib imagemagick ]);
	};

    build =
      { system ? builtins.currentSystem }:

      let
	pkgs = import <nixpkgs> { inherit system; };
        greader = (import <guile-reader/release.nix>).build {
          inherit system;
        };
      in
	pkgs.releaseTools.nixBuild {
	  name = "skribilo";
	  src = jobs.tarball;
	  buildInputs = [ greader ]
            ++ (with pkgs; [ guile guile_lib imagemagick ploticus lout ]);
	  inherit buildOutOfSourceTree;
	};

    build_without_lout =
      { system ? builtins.currentSystem }:

      let
	pkgs = import <nixpkgs> { inherit system; };
        greader = (import <guile-reader/release.nix>).build {
          inherit system;
        };
      in
	pkgs.releaseTools.nixBuild {
	  name = "skribilo-without-lout";
	  src = jobs.tarball;
	  buildInputs = [ greader ]
            ++ (with pkgs; [ guile guile_lib imagemagick ploticus ]);
	  inherit buildOutOfSourceTree;
	};

    build_guile18 =
      { system ? builtins.currentSystem }:

      let
	pkgs = import <nixpkgs> { inherit system; };
        greader = (import <guile-reader/release.nix>).build_guile18 {
          inherit system;
        };
      in
	pkgs.releaseTools.nixBuild {
	  name = "skribilo-guile1.8";
	  src = jobs.tarball;
	  buildInputs = [ greader ]
            ++ (with pkgs; [ guile_1_8 imagemagick ploticus lout ]);
	  inherit buildOutOfSourceTree;
	};
  };
in
  jobs
