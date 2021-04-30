pipeline {
    agent {
        label "guixsd"
    }
    stages {
        stage("Invoking guix build") {
            steps {
                script {
                    writeFile file: "channels.scm", text: "%default-channels"
                    output = sh (script: (["guix", "time-machine",
                                           "--channels=channels.scm",
                                           "--commit=d68de958b60426798ed62797ff7c96c327a672ac",
                                           "--url=${library('jenkins-wi-shared-library').Constants.gitGuixUrl}",
                                           "--", "build", "-f", "guix.scm"].join(" ")),
                                 returnStdout: true)
                }
            }
        }
        stage("Install on nodes") {
            steps {
                sh "guix copy --from=spb ${output}"
                sh "guix install ${output}"
            }
        }
    }
    post {
        always {
            sendNotifications currentBuild.result
        }
    }
}
