

buildMvn {
  publishModDescriptor = true
  mvnDeploy = true
  doKubeDeploy = true
  buildNode = 'jenkins-agent-java21'

  doDocker = {
    buildJavaDocker {
      publishMaster = true
      // healthChk has been moved into PermsIT.java
    }
  }
}
