resolvers += Resolver.url("hmrc-sbt-plugin-releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

//resolvers += "hmrc-releases" at "https://nexus-preview.tax.service.gov.uk" + "/content/repositories/wso2-releases"

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "1.4.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-git-versioning" % "0.9.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "1.0.0")

// Added link regarding latest version of Play
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.12")
