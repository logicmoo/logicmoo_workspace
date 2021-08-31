
# mount -o bind,rw `find /home/gitlab-runner/builds/ -name logicmoo_workspace` /var/lib/jenkins/workspace/logicmoo_workspace
# chown jenkins.gitlab-runner -R /var/lib/jenkins/workspace/logicmoo_workspace
# chmod g+rwx -R /var/lib/jenkins/workspace/logicmoo_workspace
pipeline {
    agent any

    stages {
       stage('gitlab') {
          steps {
             echo 'Notify GitLab'
             updateGitlabCommitStatus name: 'build', state: 'pending'
             updateGitlabCommitStatus name: 'build', state: 'success'
          }
       }
    }
 }

