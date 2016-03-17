# Download task and access relevant information to start running experiments
\dontrun{
  task = getOMLTask(1)
  task
  task$task.type
  task$input$data.set
  head(task$input$data.set$data)
}

