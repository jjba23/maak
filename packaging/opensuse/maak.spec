#
# spec file for package maak
#
# Copyright (c) 2026 SUSE LLC and contributors
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#

Name:           maak
Version:        0.8.17
Release:        0
Summary:        Command runner à la Make using Guile Scheme
License:        GPL-3.0-or-later
URL:            https://codeberg.org/jjba23/maak
Source0:        %{name}-%{version}.tar.gz
BuildRequires:  bash-completion
BuildRequires:  fish
BuildRequires:  guile-devel
BuildRequires:  zsh
Requires:       bash
Requires:       coreutils
Requires:       guile
Requires:       util-linux

%description
Maak is an extensible command runner and control plane for your projects.
It allows you to use Guile Scheme to define tasks, build steps, and automation.

%prep
%autosetup -n %{name}-%{version}
sed -i '1s|^#!%{_bindir}/env \(.*\)$|#!%{_bindir}/\1|' scripts/maak
sed -i '1s|^#!%{_bindir}/env \(.*\)$|#!%{_bindir}/\1|' scripts/maak-completion.bash
sed -i '1s|^#!%{_bindir}/env \(.*\)$|#!%{_bindir}/\1|' scripts/maak-completion.fish
sed -i '1s|^#!%{_bindir}/env \(.*\)$|#!%{_bindir}/\1|' scripts/maak-completion.zsh

%build

%install
SITE_DIR="%{buildroot}%{_datadir}/guile/site/3.0"
CCACHE_DIR="%{buildroot}%{_libdir}/guile/3.0/site-ccache"

mkdir -p "$SITE_DIR/maak"
mkdir -p "$CCACHE_DIR/maak"

if [ -d "src/maak" ]; then
  cp -r src/maak/* "$SITE_DIR/maak/"
elif [ -d "src" ]; then
  cp -r src/* "$SITE_DIR/maak/"
elif [ -d "maak" ]; then
  cp -r maak/* "$SITE_DIR/maak/"
else
  find . -maxdepth 1 -name "*.scm" -exec cp {} "$SITE_DIR/maak/" \;
fi

find "$SITE_DIR/maak" -name "*.scm" | while read -r scm_file; do
  rel_path="${scm_file#$SITE_DIR/}"
  go_file="$CCACHE_DIR/${rel_path%.scm}.go"
  mkdir -p "$(dirname "$go_file")"
  guild compile -L "$SITE_DIR" -o "$go_file" "$scm_file"
done

install -D -m 0755 scripts/maak %{buildroot}%{_bindir}/maak
install -D -m 0644 resources/help.txt %{buildroot}%{_datadir}/resources/help.txt

install -D -m 0755 scripts/maak-completion.bash %{buildroot}%{_datadir}/bash-completion/completions/maak
install -D -m 0755 scripts/maak-completion.fish %{buildroot}%{_datadir}/fish/vendor_completions.d/maak.fish
install -D -m 0755 scripts/maak-completion.zsh %{buildroot}%{_datadir}/zsh/site-functions/_maak

%files
%license COPYING
%doc README.org
%{_bindir}/maak
%dir %{_datadir}/guile
%dir %{_datadir}/guile/site
%{_datadir}/guile/site/3.0/
%dir %{_datadir}/guile/site/3.0/maak

%dir %{_libdir}/guile
%dir %{_libdir}/guile/3.0
%dir %{_libdir}/guile/3.0/site-ccache
%{_libdir}/guile/3.0/site-ccache/maak

%dir %{_datadir}/resources
%{_datadir}/resources/help.txt

%{_datadir}/bash-completion/completions/maak
%{_datadir}/fish/vendor_completions.d/maak.fish
%{_datadir}/zsh/site-functions/_maak

%changelog
