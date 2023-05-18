get_table_headers <- function() {
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 2, ''),
        th(colspan = 10, 'Home'),
        th(colspan = 10, 'Away'),
        th(colspan = 6, 'Overall')
      ),
      tr(
        lapply(names(get_ssn_records("2022/23")), th)
      )
    )
  ))

  return (sketch)
}
