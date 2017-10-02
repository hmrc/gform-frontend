# gform-frontend

[![Build Status](https://travis-ci.org/hmrc/gform-frontend.svg)](https://travis-ci.org/hmrc/gform-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/gform-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/gform-frontend/_latestVersion)

Gform is a system for offering more straightforward tax forms online using based on a template of the form in json.
 
This frontend presents those forms based on templates fetched from the backend.  It handles any authorisation using 
either eeitt authorisation or government gateway, when required it can also manage enrolment for government gateway.
Data values entered on the forms are persisted by the backend and ultimately submitted to DMS by the backend.

### Running Gform Locally with Service Manager

When running locally: 
 
    sm --start GFORM_DEP -f
    
runs all dependent services of gform. 

when using gform: 
    
    sm --start GFORM_ALL -f
    
run dependencies and gform.

### Enter a form

To try entering a form locally, with sample data in the gform backend and using the auth-login-stub, browse to:

[(http://localhost:9949/auth-login-stub/gg-sign-in?continue=http://localhost:9195/submissions/new-form/aaa999)](http://localhost:9949/auth-login-stub/gg-sign-in?continue=http://localhost:9195/submissions/new-form/aaa999)

to use this form you will have to upload a template see below. 

### Uploading sample data

Upload a form template:

    curl http://localhost:9196/gform/formtemplates -H "Content-Type: application/json" -d '@sample-data/template-aaa999.json'

this template is in json and has several required fields. 

Upload eeitt test data for legacy eeitt auth if assigned in template: 
    
    curl --data-binary '@sample-data/EEITTTestUsers.txt' http://localhost:9191/eeitt/etmp-data/live/business-users
        
 the backend can be accessed through the frontend proxy, to try this locally:

    curl -s http://localhost:9195/submissions/test-only/proxy-to-gform/gform/formtemplates -H "Content-Type: application/json" -H "X-requested-with: foo" -d '@sample-data/template-aaa999.json'
    
    you can proxy to any backend call. only when test only routes are enabled.
    
(Note that you will need to have configured your local gform-frontend for test only routes and CSRF bypass, as in for example app-config-dev

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html")
