# gform-frontend

[![Build Status](https://travis-ci.org/hmrc/gform-frontend.svg)](https://travis-ci.org/hmrc/gform-frontend) [ ![Download](https://api.bintray.com/packages/hmrc/releases/gform-frontend/images/download.svg) ](https://bintray.com/hmrc/releases/gform-frontend/_latestVersion)

Gforms is a SaaS for authoring and running form submission journeys.
 
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
Note you need to be in the gform project.

    curl http://localhost:9196/gform/formtemplates -H "Content-Type: application/json" -d '@sample-data/template-aaa999.json'

this template is in json and has several required fields. 

Upload eeitt test data for legacy eeitt auth if assigned in template: 
    
    curl --data-binary '@sample-data/EEITTTestUsers.txt' http://localhost:9191/eeitt/etmp-data/live/business-users
        
 the backend can be accessed through the frontend proxy, to try this locally:

    curl -s http://localhost:9195/submissions/test-only/proxy-to-gform/gform/formtemplates -H "Content-Type: application/json" -H "X-requested-with: foo" -d '@sample-data/template-aaa999.json'
    
    you can proxy to any backend call. only when test only routes are enabled.
    
(Note that you will need to have configured your local gform-frontend for test only routes and CSRF bypass, as in for example app-config-dev


## Getting started using postman

1. Start the gform and gform-frontend microservices, and any dependencies, as per "Running gform locally with service manager" above.
2. Use postman to post form JSON to the gform microservice.  If using postman, you can import the settings and a sample form definition from [here](https://www.getpostman.com/collections/e77f465bb51501554e15").  Go to the Collections tab on the left and click on "POST a sample gform" under the "gform" folder.
3. Click on the "Body" tab found under the URL box.  This contains the JSON for a working sample form.  Change the _id and name at the top of the sample form to your own unique ID e.g. change test-form-CHANGE-MY-ID to mytest-form-ND.
4. Click the Send button to send your form specification to gforms running in an MDTP environment.  If you get a status 204 back with no response this means the form has been successfully validated and is ready to use.
5. Ensure you are connected to the VPN then access the form in an MDTP environment via the following URL - but note you must update the form ID at the end of this link to the ID that you set in step (3) above.  Don't need to set anything in auth wizard just click Submit:
  https://<MDTP environment host>/auth-login-stub/gg-sign-in?continue=https%3A%2F%2F<MDTP environment host>%2Fsubmissions%2Fnew-form%2Fsample-XXX
6. Once you can see your new form is working you can refer to the specification and re-post updates to your form JSON to tailor your test form to your requirements.  Most types of updates are applied instantly to journeys in-progress but some will require you to re-start the journey.  If you make any mistakes you will get a 400 Bad Request with details of the error in the body.
  You may want to consider using an online JSON editor like https://jsonblob.com/, https://jsoneditoronline.org/ or an editor like https://atom.io/ to simplify authoring your JSON form definitions and copy/paste to postman to apply changes. You can get a feel for the gform template JSON structure by referring to the specification and [current examples](https://github.com/hmrc/gform-templates).


### Using legacy-eeitt-auth?
> If you are working on an EEITT form and using { "authModule": "legacyEEITTAuth" }
> then you will be prompted to login with an enrolled reference number and postcode.
> Pick a test user to use for the correct regime (note 3rd and 4th characters of the ID)
> from the list of test users at https://github.com/hmrc/gform/blob/master/sample-data/EEITTTestUsers.txt.


### License

This code is open source software licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html).
