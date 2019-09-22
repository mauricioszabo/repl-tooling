#!/bin/bash

if [ ! -z "$TRAVIS_TAG" ]
then
    echo "deploying a new version"
else
    echo "deploying a snapshot version"
    lein change version str "-SNAPSHOT"
fi

lein change :deploy-repositories concat '[["releases" {:url "https://clojars.org/repo/" :username :env/clojars_login :password :env/clojars_password}]]'
lein deploy releases
