#!/bin/bash

TOOLS=`dirname $0`

echo $DB

source tools/travis-common-vars.sh

SQLDIR=${BASE}/apps/ejabberd/priv

TRAVIS_DB_PASSWORD=$(cat /tmp/travis_db_password)

if [ $DB = 'mysql' ]; then
    echo "Configuring mysql"
    sudo service mysql stop || echo "Failed to stop mysql"
    docker run -d \
        -e MYSQL_ROOT_PASSWORD=secret \
        -e MYSQL_DATABASE=ejabberd \
        -e MYSQL_USER=ejabberd \
        -e MYSQL_PASSWORD=$TRAVIS_DB_PASSWORD \
        -v ${SQLDIR}/mysql.sql:/docker-entrypoint-initdb.d/mysql.sql:ro \
        -p 3306:3306 --name=mongooseim-mysql mysql

elif [ $DB = 'pgsql' ]; then
    echo "Configuring postgres"
    psql -U postgres -c "CREATE ROLE ejabberd PASSWORD '${TRAVIS_DB_PASSWORD}' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;"
    psql -U postgres -c "CREATE DATABASE ejabberd;"
    echo "Creating schema"
    psql -U postgres -q -d ejabberd -f ${SQLDIR}/pg.sql

elif [ $DB = 'riak' ]; then
    # Make riak image with search enabled
    docker build -t riak $(pwd)/tools/docker/riak
    docker run -d -p 8087:8087 -p 8098:8098 \
        -e DOCKER_RIAK_BACKEND=leveldb \
        -e DOCKER_RIAK_CLUSTER_SIZE=1 \
        --name=mongooseim-riak riak
    tools/wait_for_service.sh mongooseim-riak 8098 || docker logs riak
    tools/setup_riak

elif [ $DB = 'cassandra' ]; then
    docker run -d -p 9042:9042 -e MAX_HEAP_SIZE=128M -e HEAP_NEWSIZE=64M --name=cassandra cassandra:${CASSANDRA_VERSION}
    tools/wait_for_service.sh cassandra 9042 || docker logs cassandra

    # Deleted --rm on travis for speedup
    docker run -it -v "$(pwd)/apps/ejabberd/priv/cassandra.cql:/cassandra.cql:ro" \
        --link cassandra:cassandra \
        cassandra:${CASSANDRA_VERSION} \
        sh -c 'exec cqlsh "$CASSANDRA_PORT_9042_TCP_ADDR" -f /cassandra.cql'

elif [ $DB = 'mssql' ]; then
    CONT_NAME=mongooseim-mssql
    echo "'SA_PASSWORD=${TRAVIS_DB_PASSWORD}'"
    docker run -e 'ACCEPT_EULA=Y' -e "SA_PASSWORD=${TRAVIS_DB_PASSWORD}" \
        -p 1433:1433 -d \
        --name=${CONT_NAME} michalwski/mssql-server-linux-with-tools
    docker ps
    tools/wait_for_service.sh ${CONT_NAME} 1433 || docker logs ${CONT_NAME}
    # docker start ${CONT_NAME} && /
    # tools/wait_for_service.sh ${CONT_NAME} 1433 || docker logs ${CONT_NAME}
    docker exec -it ${CONT_NAME} sqlcmd -S localhost -U SA -P ${TRAVIS_DB_PASSWORD} -Q "CREATE DATABASE ejabberd;"
    docker cp ${SQLDIR}/mssql2012.sql ${CONT_NAME}:mssql2012.sql
    docker exec -it ${CONT_NAME} sqlcmd -S localhost -U SA -P ${TRAVIS_DB_PASSWORD} -i mssql2012.sql

    read -d '' odbcini << EOL
[ODBC Data Sources]
MongooseIMmssql = MSSQL linux

[MongooseIMmssql]
Description = TDS driver (Sybase/MS SQL)
Driver = /usr/local/lib/libtdsodbc.so.0
Servername           = MSSQLlinux
Database             = ejabberd
TDS_Version          = 7.4
Charset              = UTF8
EOL
    echo "building freetds"
    git clone https://github.com/FreeTDS/freetds.git
    cd freetds
    ./autogen.sh > /dev/null 2>&1
    make > /dev/null 2>&1
    sudo make install > /dev/null 2>&1
    echo "~/.odbc.ini"
    echo "$odbcini" | sudo tee /etc/odbc.ini

    read -d '' freetds << EOL
[MSSQLlinux]
host = localhost
port = 1433
tds version = 7.4
client sharset = UTF-8
EOL
    echo "~/.freetds.conf"
    echo "$freetds" > ~/.freetds.conf

    echo "SELECT NAME from sys.Databases;" | isql -v MongooseIMmssql SA ${TRAVIS_DB_PASSWORD}
    echo "insert into users(username, password) values ('bob', 'makrolika');" | isql -v MongooseIMmssql SA ${TRAVIS_DB_PASSWORD}
    echo "select * from users;" | isql -v MongooseIMmssql SA ${TRAVIS_DB_PASSWORD}

fi
