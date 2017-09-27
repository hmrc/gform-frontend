# gform-frontend

[![Build Status](https://travis-ci.org/hmrc/gform-frontend.svg)](https://travis-ci.org/hmrc/gform-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/gform-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/gform-frontend/_latestVersion)

### Enter a form

To try entering a form locally, with sample data in the gform backend and using the auth-login-stub, browse to:

[(http://localhost:9949/auth-login-stub/gg-sign-in?continue=http://localhost:9195/submissions/new-form/IPT100)](http://localhost:9949/auth-login-stub/gg-sign-in?continue=http://localhost:9195/submissions/new-form/IPT100)

### Uploading sample data

Upload a form template:

    curl http://localhost:9196/gform/formtemplates -H "Content-Type: application/json" -d '@sample-data/template-aaa999.json'

Upload eeitt test data: 
    
    curl --data-binary '@sample-data/EEITTTestUsers.txt' http://localhost:9191/eeitt/etmp-data/live/business-users
        
Aside from a local service, the backend needs to be accessed through the frontend proxy, to try this locally:

    curl -s http://localhost:9195/submissions/test-only/proxy-to-gform/gform/formtemplates -H "Content-Type: application/json" -H "X-requested-with: foo" -d '@sample-data/template-aaa999.json'
    
    you can proxy to any backend call. only when application.route=testOnlyDoNotUseInAppConfig.routes
    
(Note that you will need to have configured your local gform-frontend for test only routes and CSRF bypass, as in for example app-config-dev

When running locally: 
 
    sm --start GFORM_DEP -f
    
 runs all dependent services of gform. 

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")
