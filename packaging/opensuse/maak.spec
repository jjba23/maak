Name: maak
Version: 0.8.6
Release: 0
Summary: Command runner à la Make using Guile Scheme
License: GPL-3.0-or-later
Group: Development/Tools/Building
URL: https://codeberg.org/jjba23/maak
Source0: %{name}-%{version}.tar.gz

BuildRequires: guile-devel
Requires: bash
Requires: coreutils
Requires: guile3
Requires: util-linux

%description
Maak is an extensible command runner and control plane for your projects.
It allows you to use Guile Scheme to define tasks, build steps, and automation.

%prep
%autosetup -n %{name}-%{version}

%build
# Define target paths relative to build root for compilation
SITE_DIR="%{buildroot}%{_datadir}/guile/site/3.0"
CCACHE_DIR="%{buildroot}%{_libdir}/guile/3.0/site-ccache"

mkdir -p "$SITE_DIR/maak"
mkdir -p "$CCACHE_DIR/maak"

# Copy scheme module sources
if [ -d "src/maak" ]; then
  cp -r src/maak/* "$SITE_DIR/maak/"
elif [ -d "src" ]; then
  cp -r src/* "$SITE_DIR/maak/"
elif [ -d "maak" ]; then
  cp -r maak/* "$SITE_DIR/maak/"
else
  find . -maxdepth 1 -name "*.scm" -exec cp {} "$SITE_DIR/maak/" \;
fi

# Byte-compile Scheme modules using guild
find "$SITE_DIR/maak" -name "*.scm" | while read -r scm_file; do
  rel_path="${scm_file#$SITE_DIR/}"
  go_file="$CCACHE_DIR/${rel_path%.scm}.go"
  mkdir -p "$(dirname "$go_file")"
  guild compile -L "$SITE_DIR" -o "$go_file" "$scm_file"
done

%install
# Install the main CLI executable wrapper
install -D -m 0755 scripts/maak %{buildroot}%{_bindir}/maak

# Install help resources expected by the wrapper script
install -D -m 0644 resources/help.txt %{buildroot}%{_datadir}/resources/help.txt

# Install shell completions for Bash, Fish, and Zsh
install -D -m 0644 scripts/maak-completion.bash %{buildroot}%{_datadir}/bash-completion/completions/maak
install -D -m 0644 scripts/maak-completion.fish %{buildroot}%{_datadir}/fish/vendor_completions.d/maak.fish
install -D -m 0644 scripts/maak-completion.zsh %{buildroot}%{_datadir}/zsh/site-functions/_maak

%files
%{_bindir}/maak
%{_datadir}/guile/site/3.0/maak
%{_libdir}/guile/3.0/site-ccache/maak
%{_datadir}/resources/help.txt
%{_datadir}/bash-completion/completions/maak
%{_datadir}/fish/vendor_completions.d/maak.fish
%{_datadir}/zsh/site-functions/_maak

%changelog
