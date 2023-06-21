

buildMvn {
  publishModDescriptor = true
  mvnDeploy = true
  doKubeDeploy = true
  buildNode = 'jenkins-agent-java17'

  doDocker = {
    buildJavaDocker {
      publishMaster = true
      // healthChk has been moved into PermsIT.java
    }
  }
}
