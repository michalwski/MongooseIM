#!/bin/bash

# travis secure envs are available only of our own PRs

if [ ${TRAVIS_SECURE_ENV_VARS} == 'true' ]; then


./tools/configure with-all
make rel

MIM_TAR_FULL_NAME=mongooseim-$TRAVIS_BRANCH.OTP-$TRAVIS_OTP_RELEASE.$(lsb_release -is | tr "A-Z" "a-z").$(lsb_release -rs).$(uname -m).tar.bz2

MONGOOSE_TGZ=mongooseim.tar.gz

tar -cjh --transform='s,rel/mongooseim,mongooseim-${$TRAVIS_BRANCH},S' -f ${MIM_TAR_FULL_NAME} rel/mongooseim
tar czh --transform='s,rel/mongooseim,mongooseim,S' -f $MONGOOSE_TGZ rel/mongooseim

export BUILDS=`pwd`
export MEMBER_TGZ=mongooseim.tar.gz

DOCKERHUB_TAG=${TRAVIS_BRANCH}

if [ ${TRAVIS_PULL_REQUEST} != 'false' ]; then
    DOCKERHUB_TAG="PR-${TRAVIS_PULL_REQUEST}"
elif [ ${TRAVIS_BRANCH} == 'master' ]; then
    DOCKERHUB_TAG="latest";
fi

git clone https://github.com/michalwski/mongooseim-docker.git
cd mongooseim-docker

cp ../${MONGOOSE_TGZ} member

docker build -f Dockerfile.member -t mongooseim .

cd ../

docker run -t -d -h mongooseim-1 --name mongooseim-1 -p 5222:5222 mongooseim

tools/wait_for_service.sh mongooseim-1 5123
if [ $? -ne 0 ]; then
  docker logs mongooseim-1
  exit 1
fi

#fail if pushing to docker fails
set -e

docker login -e=${DOCKERHUB_EMAIL} -u ${DOCKERHUB_USER} -p ${DOCKERHUB_PASS}

docker tag mongooseim ${DOCKERHUB_USER}/mongooseim:${DOCKERHUB_TAG}

docker push ${DOCKERHUB_USER}/mongooseim:${DOCKERHUB_TAG}

fi
