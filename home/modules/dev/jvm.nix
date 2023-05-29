{ pkgs, config, ... }:

let
  sbtBaseConfigPath = config.programs.sbt.baseUserConfigPath;
  jdk = pkgs.jdk11;
  sbt-extras = pkgs.sbt-extras.override { inherit jdk; };
in
{
  home.packages = with pkgs; [
    coursier
    maven
    visualvm
    unstable.scala-cli
  ];

  # Write manually what can’t be handled by home-manager options
  home.file = {
    "${sbtBaseConfigPath}/reload-on-changes.sbt" = {
      text = "Global / onChangedBuildSource := ReloadOnSourceChanges";
    };
    "${sbtBaseConfigPath}/plugins/dependency-tree.sbt" = {
      text = "addDependencyTreePlugin";
    };
  };

  programs = {
    java = {
      enable = true;
      package = jdk;
    };

    sbt = {
      enable = true;
      package = sbt-extras;
      plugins = [
        {
          org = "com.typesafe.sbt";
          artifact = "sbt-git";
          version = "1.0.2";
        }
        {
          org = "com.timushev.sbt";
          artifact = "sbt-updates";
          version = "0.6.1";
        }
        {
          org = "io.github.todokr";
          artifact = "sbt-project-switcher";
          version = "0.1.4";
        }
      ];
    };
  };
}
