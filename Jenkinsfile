

buildMvn {
  publishModDescriptor = true
  mvnDeploy = true
  doKubeDeploy = true
  buildNode = 'jenkins-agent-java11'

  doApiLint = true
  doApiDoc = true
  apiTypes = 'RAML'
  apiDirectories = 'ramls'

  doDocker = {
    buildJavaDocker {
      publishMaster = true
      // healthChk has been moved into PermsIT.java
    }
  }
}

