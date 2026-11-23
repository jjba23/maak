# maak
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# A formula, not a cask and not a gem: maak is a command-line program written in
# Guile.  A cask installs a prebuilt macOS bundle, which maak is not, and a gem
# is Ruby code for Ruby's own package manager, which maak is not either.  A
# formula is the one that builds the sources with the Guile it depends on.
#
# The version is a copy of `maak-version' in this file's url, written by
# `maak sync-version'.  The checksum is refreshed from the published archive
# after each release, the one thing no release task can know, exactly as the
# Arch recipe's is.
class Maak < Formula
  desc "Command runner à la Make using Guile Scheme"
  homepage "https://github.com/jjba23/maak"
  url "https://github.com/jjba23/maak/archive/refs/tags/v1.2.1.tar.gz"
  sha256 "d91d88c351955d7b2de97e42ea53910090a343224f6a1d22b6409cc181aa7566"
  license "MIT"

  depends_on "coreutils"
  depends_on "gnu-getopt"
  depends_on "guile"

  def install
    # The launcher resolves the library, the help screen and the manual page
    # relative to its own directory, so they live together under libexec and
    # only the wrappers go in bin.
    bin_dir = libexec/"bin"
    site = libexec/"share/guile/site/3.0"
    cache = libexec/"lib/guile/3.0/site-ccache"

    site.install "src/maak"
    (libexec/"share/resources").install "resources/help.txt"

    # Byte-compile into the Cellar, so a first run is not a compile.
    cache.mkpath
    site.glob("maak/**/*.scm").sort.each do |source|
      target = cache/source.relative_path_from(site).sub_ext(".go")
      target.dirname.mkpath
      system Formula["guile"].opt_bin/"guild", "compile",
             "-L", site.to_s, "-o", target.to_s, source.to_s
    end

    bin_dir.install "scripts/maak" => "maak"
    bin_dir.install "contrib/bin/maak-tasks" => "maak-tasks"
    bin_dir.install "contrib/zed/maak-zed" => "maak-zed"

    man1.install "man/maak.1"
    bash_completion.install "scripts/maak-completion.bash" => "maak"
    fish_completion.install "scripts/maak-completion.fish" => "maak.fish"
    zsh_completion.install "scripts/maak-completion.zsh" => "_maak"

    # The launcher needs GNU getopt and a GNU userland to be the same program on
    # macOS as it is on Linux, and the helper needs the library's load path.
    path = [
      "#{Formula["coreutils"].opt_libexec}/gnubin",
      Formula["gnu-getopt"].opt_bin,
      Formula["guile"].opt_bin,
      "$PATH",
    ].join(":")
    %w[maak maak-tasks maak-zed].each do |name|
      (bin/name).write <<~SH
        #!/bin/bash
        export PATH="#{path}"
        export GUILE_LOAD_PATH="#{site}"
        export GUILE_LOAD_COMPILED_PATH="#{cache}"
        exec "#{bin_dir/name}" "$@"
      SH
      (bin/name).chmod 0755
    end
  end

  test do
    (testpath/"maak.scm").write <<~SCHEME
      (define-module (maak)
        #:use-module (maak maak)
        #:export (hello))

      (define (hello)
        "Say hello."
        (log-info "hello"))
    SCHEME

    assert_match "maak #{version}", shell_output("#{bin}/maak --version")
    assert_match "hello", shell_output("#{bin}/maak --list")
    assert_match "hello", shell_output("#{bin}/maak-tasks --directory #{testpath}")
  end
end
