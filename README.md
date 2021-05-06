### MODE:  An application to visualize omics data in trelliscope.
****

Omics-data objects from the pmartR R package are ingested and sent to a separate R process to create trelliscope displays, which are then retrieved and visualized in the app.

****

#### Docker container

To build the container, you need to put the following binaries in ./local_packages next to the Dockerfile:

- trelliscopejs 0.2.2 (trelliscopejs_0.2.2.tar.gz)
- mapDataAccess 0.0.0.9000 (mapDataAccess_0.0.0.9000.tar.gz)
- rworker 0.1.2 (rworker_0.1.2.tar.gz)

The Dockerfile looks for a folder local_packages to copy into the container.  Then simply run the standard build command:

docker build -t image:tag  .

To run the container, we mount two config files, one for minio and one for redis.  Examples can be found in the cfg folder of this repo.

Run command looks like:

docker run -p 5601:5601 -v /some/path/minio_config_svc.yml:/minio_config.yml -v /some/path/redis_config.yml:/redis_config.yml
