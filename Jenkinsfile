#!groovy

// Args:
// GitHub repo name
// Jenkins agent label
// Tracing artifacts to be stored alongside build logs
// Optional: artifacts to cache between the builds
pipeline("dmt_core", 'docker-host', "_build/") {

    runStage('compile') {
     sh 'make compile'
    }

    runStage('xref') {
     sh 'make xref'
    }

    runStage('dialyze') {
     sh 'make dialyze'
    }
}
