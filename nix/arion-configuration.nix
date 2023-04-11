_:

{
  perSystem.arionProjectConfiguration = { pkgs, ... }: {
    project.name = "beckn-gateway";
    docker-compose.raw.volumes = {
      pgadmin-data = null;
    };
    services = {
      registry-db.service = {
        image = "postgis/postgis:12-3.0";
        container_name = "atlas-dev-registry";
        ports = [ "5438:5432" ];
        volumes = [
          "${../dev/sql-seed/mock-registry-seed.sql}:/docker-entrypoint-initdb.d/1-mock-registry-seed.sql:Z"
          "${../dev/local-testing-data/mock-registry.sql}:/docker-entrypoint-initdb.d/2-mock-registry-testing-data.sql:Z"
        ];
        environment = {
          POSTGRES_DB = "atlas_dev";
          POSTGRES_USER = "atlas";
          POSTGRES_PASSWORD = "atlas";
          POSTGRES_HOST_AUTH_METHOD = "scram-sha-256";
          POSTGRES_INITDB_ARGS = "--auth=scram-sha-256";
        };
      };
      registry-redis.service = {
        image = "redis:5";
        ports = [ "6579:6379" ];
      };
      registry-pg-admin.service = {
        image = "dpage/pgadmin4";
        ports = [ "9401:80" ];
        environment = {
          PGADMIN_DEFAULT_EMAIL = "root@localhost.localdomain";
          PGADMIN_DEFAULT_PASSWORD = "secret";
          PGADMIN_DISABLE_POSTFIX = "true";
          PGADMIN_CONFIG_SERVER_MODE = "False";
        };
        volumes = [
          "pgadmin-data:/var/lib/pgadmin"
          "${../dev/pgadmin/servers.json}:/pgadmin4/servers.json"
        ];
        profiles = [ "pgadmin" ];
      };
    };
  };
}
