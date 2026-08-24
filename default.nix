# maak

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# maak is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# maak is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with maak.  If not, see <https://www.gnu.org/licenses/>.

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, stdenv ? pkgs.stdenv
, guile ? pkgs.guile
, coreutils ? pkgs.coreutils
, bash ? pkgs.bash
, util-linux ? pkgs.util-linux
, makeWrapper ? pkgs.makeWrapper
}:

stdenv.mkDerivation {
  pname = "maak";
  version = "0.8.17";

  src = ./.;

  nativeBuildInputs = [
    guile
    makeWrapper
  ];

  buildInputs = [
    guile
    coreutils
    util-linux
    bash
  ];

  buildPhase = ''
    runHook preBuild

    SITE_DIR="$out/share/guile/site/3.0"
    CCACHE_DIR="$out/lib/guile/3.0/site-ccache"

    mkdir -p "$SITE_DIR" "$CCACHE_DIR"

    # Copy Scheme sources into $SITE_DIR/maak preserving module structure
    if [ -d "src/maak" ]; then
      cp -r src/maak "$SITE_DIR/"
    elif [ -d "src" ]; then
      mkdir -p "$SITE_DIR/maak"
      cp -r src/* "$SITE_DIR/maak/"
    elif [ -d "maak" ]; then
      cp -r maak "$SITE_DIR/"
    else
      mkdir -p "$SITE_DIR/maak"
      find . -maxdepth 1 -name "*.scm" -exec cp {} "$SITE_DIR/maak/" \;
    fi

    # Export load paths so guild compile can resolve (maak ...) cross-module imports
    export GUILE_LOAD_PATH="$SITE_DIR:$GUILE_LOAD_PATH"
    export GUILE_LOAD_COMPILED_PATH="$CCACHE_DIR:$GUILE_LOAD_COMPILED_PATH"

    # Dynamically byte-compile all Scheme module files (.scm -> .go)
    cd "$SITE_DIR"
    find maak -name "*.scm" | while read -r scm_file; do
      go_file="$CCACHE_DIR/''${scm_file%.scm}.go"
      mkdir -p "$(dirname "$go_file")"
      guild compile -L "$SITE_DIR" -o "$go_file" "$scm_file" || true
    done
    cd -

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    # Install executable wrapper script
    mkdir -p $out/bin
    if [ -f "scripts/maak" ]; then
      cp scripts/maak $out/bin/maak
    elif [ -f "maak" ]; then
      cp maak $out/bin/maak
    fi
    chmod +x $out/bin/maak

    # Install resources
    mkdir -p $out/share/resources
    if [ -f "resources/help.txt" ]; then
      cp resources/help.txt $out/share/resources/
    fi

    # Locate shell completion scripts
    copy_completion() {
      local src_file=$1
      local dest_path=$2
      if [ -f "scripts/$src_file" ]; then
        cp "scripts/$src_file" "$dest_path"
      elif [ -f "$src_file" ]; then
        cp "$src_file" "$dest_path"
      fi
    }

    # Install completion scripts for Bash, Fish, and Zsh
    mkdir -p $out/share/bash-completion/completions
    mkdir -p $out/share/fish/vendor_completions.d
    mkdir -p $out/share/zsh/site-functions

    copy_completion "maak-completion.bash" "$out/share/bash-completion/completions/maak"
    copy_completion "maak-completion.fish" "$out/share/fish/vendor_completions.d/maak.fish"
    copy_completion "maak-completion.zsh" "$out/share/zsh/site-functions/_maak"

    # Wrap binary with required PATH and Guile module search paths
    wrapProgram $out/bin/maak \
      --prefix PATH : ${lib.makeBinPath [ guile coreutils util-linux bash ]} \
      --prefix GUILE_LOAD_PATH : "$out/share/guile/site/3.0" \
      --prefix GUILE_LOAD_COMPILED_PATH : "$out/lib/guile/3.0/site-ccache"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Command runner à la Make using Guile Scheme";
    homepage = "https://codeberg.org/jjba23/maak";
    license = licenses.gpl3Plus;
    platforms = platforms.linux ++ platforms.darwin;
    mainProgram = "maak";
  };
}
