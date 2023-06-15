{ self, ... }:

let
  imageName = "ghcr.io/nammayatri/beckn-gateway";
  # self.rev will be non-null only when the working tree is clean
  # This is equivalent to `git rev-parse --short HEAD`
  imageTag = builtins.substring 0 6 (self.rev or "dev");
in
{
  config = {
    perSystem = { self', pkgs, lib, ... }: {
      packages = {
        dockerImage =
          let
            beckn-gateway = lib.getBin self'.packages.beckn-gateway;

            # Wrap the package so that its binaries are in /opt/app.
            #
            # Rationale: Our k8s deployment config is hardcoded to look for exes
            # under /opt/app
            beckn-gateway-in-opt = pkgs.symlinkJoin {
              name = "beckn-gateway-exes-opt";
              paths = [ beckn-gateway ];
              postBuild = ''
                mkdir $out/opt && mv $out/bin $out/opt/app
              '';
            };
          in
          pkgs.dockerTools.buildImage {
            name = imageName;
            created = "now";
            tag = imageTag;
            copyToRoot = pkgs.buildEnv {
              paths = with pkgs; [
                cacert
                awscli
                coreutils
                bash
                # Add project root to paths to copy dhall-configs and swagger dirs
                self
                beckn-gateway-in-opt
              ];
              name = "beckn-gateway";
              pathsToLink = [
                "/bin"
                "/opt"
              ];
            };
            config = {
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                # Ref: https://hackage.haskell.org/package/x509-system-1.6.7/docs/src/System.X509.Unix.html#getSystemCertificateStore
                "SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
              Cmd = [ "${lib.getExe beckn-gateway}" ];
            };

            # Test that the docker image contains contents we expected for
            # production.
            extraCommands = ''
              # Executables are under /opt/app
              ls opt/app/beckn-gateway-exe
            '';
          };
      };
    };
  };
}
