pkgs: overlays: devShell: flake: projectName: project: self: tools: {
  inherit overlays devShell;

  packages = flake.packages // {
    gcroot = pkgs.linkFarmFromDrvs "${projectName}-shell-gcroot" [
      devShell
      devShell.stdenv
      project.plan-nix
      project.roots

      (
        let
          compose = f: g: x: f (g x);
          flakePaths = compose pkgs.lib.attrValues (pkgs.lib.mapAttrs
            (name: flake: {
              name = name;
              path = flake.outPath;
            }));
        in
        pkgs.linkFarm "input-flakes" (flakePaths self.inputs)
      )

      (
        let
          getMaterializers = (name: project:
            pkgs.linkFarmFromDrvs "${name}" [
              project.plan-nix.passthru.calculateMaterializedSha
              project.plan-nix.passthru.generateMaterialized
            ]);
        in
        pkgs.linkFarmFromDrvs "materializers"
          (pkgs.lib.mapAttrsToList getMaterializers ({
            ${projectName} = project;
          } // (pkgs.lib.mapAttrs (_: builtins.getAttr "project")
            (project.tools tools))))
      )
    ];
  };
}
