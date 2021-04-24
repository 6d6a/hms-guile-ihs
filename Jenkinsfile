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
                                           "--commit=5b7a1cb077931a020c0b7e3b12f12a7bda221d96",
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
