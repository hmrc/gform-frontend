#!/usr/bin/env bash
sbt -mem 3000 ~run -Dplay.http.router=testOnlyDoNotUseInAppConf.Routes