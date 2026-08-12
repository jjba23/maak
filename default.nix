{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, stdenv ? pkgs.stdenv
, guile ? pkgs.guile
, coreutils ? pkgs.coreutils
, util-linux ? pkgs.util-linux
, makeWrapper ? pkgs.makeWrapper
}:

stdenv.mkDerivation rec {
  pname = "maak";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [
    guile
    makeWrapper
  ];

  buildInputs = [
    guile
    coreutils
    util-linux
  ];

  buildPhase = ''
    runHook preBuild

    # Determine scheme source directory (supports root or src/ directory layout)
    if [ -d "src/maak" ]; then
      SCHEME_SRC="src/maak"
    elif [ -d "src" ]; then
      SCHEME_SRC="src"
    else
      SCHEME_SRC="."
    fi

    # Create destination directories for Guile sources and compiled bytecode
    mkdir -p $out/share/guile/site/3.0/maak
    mkdir -p $out/lib/guile/3.0/site-ccache/maak

    # Copy scheme source files
    cp $SCHEME_SRC/dsl.scm $out/share/guile/site/3.0/maak/
    cp $SCHEME_SRC/maak.scm $out/share/guile/site/3.0/maak/
    cp $SCHEME_SRC/main.scm $out/share/guile/site/3.0/maak/

    # Byte-compile Guile Scheme modules (.scm -> .go)
    guild compile -L $out/share/guile/site/3.0 -o $out/lib/guile/3.0/site-ccache/maak/dsl.go $out/share/guile/site/3.0/maak/dsl.scm
    guild compile -L $out/share/guile/site/3.0 -o $out/lib/guile/3.0/site-ccache/maak/maak.go $out/share/guile/site/3.0/maak/maak.scm
    guild compile -L $out/share/guile/site/3.0 -o $out/lib/guile/3.0/site-ccache/maak/main.go $out/share/guile/site/3.0/maak/main.scm

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    # Install executable script
    mkdir -p $out/bin
    if [ -f "scripts/maak" ]; then
      cp scripts/maak $out/bin/maak
    elif [ -f "maak" ]; then
      cp maak $out/bin/maak
    fi
    chmod +x $out/bin/maak

    # Install resources (help text)
    mkdir -p $out/share/resources
    if [ -f "resources/help.txt" ]; then
      cp resources/help.txt $out/share/resources/
    fi

    # Helper function to locate and copy files from scripts/ or root
    copy_script() {
      local src_file=$1
      local dest_path=$2
      if [ -f "scripts/$src_file" ]; then
        cp "scripts/$src_file" "$dest_path"
      elif [ -f "$src_file" ]; then
        cp "$src_file" "$dest_path"
      fi
    }

    # Install shell completion scripts
    mkdir -p $out/share/bash-completion/completions
    mkdir -p $out/share/fish/vendor_completions.d
    mkdir -p $out/share/zsh/site-functions

    copy_script "maak-completion.bash" "$out/share/bash-completion/completions/maak"
    copy_script "maak-completion.fish" "$out/share/fish/vendor_completions.d/maak.fish"
    copy_script "maak-completion.zsh" "$out/share/zsh/site-functions/_maak"

    # Wrap binary with required runtime PATH and Guile module load paths
    wrapProgram $out/bin/maak \
      --prefix PATH : ${lib.makeBinPath [ guile coreutils util-linux ]} \
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
