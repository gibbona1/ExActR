function copytable(el, format) {
  var urlField = document.getElementById(el);
  var range = document.createRange();
  range.selectNode(urlField);
  window.getSelection().addRange(range);

  var copiedText;

  if (format === 'html') {
    copiedText = urlField.outerHTML;
  } else if (format === 'excel') {
    copiedText = convertTableToExcel(urlField);
  } else if (format === 'latex') {
    copiedText = convertTableToLaTeX(urlField);
  } else {
    copiedText = urlField.innerText;
  }
  
  if (copiedText) {
    //alert(copiedText)
    copyToClipboard(copiedText)
      .then(function() {
        alert('Table copied successfully as ' + format);
      })
      .catch(function(error) {
        alert('Unable to copy table: ', error);
      });
  }
  window.getSelection().removeAllRanges();
}

function convertTableToExcel(table) {
  var worksheet = XLSX.utils.table_to_sheet(table);
  var workbook = XLSX.utils.book_new();
  XLSX.utils.book_append_sheet(workbook, worksheet, 'Sheet 1');
  var excelData = XLSX.write(workbook, { type: 'binary' });

  // Convert Excel data to base64
  var array = new Uint8Array(excelData);
  var data = '';
  for (var i = 0; i < array.length; i++) {
    data += String.fromCharCode(array[i]);
  }
  var base64 = window.btoa(data);

  return base64;
}

function convertTableToLaTeX(table) {
  var latex = '';
  var rows = table.rows;
  var numRows = rows.length;
  var numCols = rows[0].cells.length;

  // Iterate over table rows
  for (var i = 0; i < numRows; i++) {
    var row = rows[i];
    // Iterate over table cells in each row
    for (var j = 0; j < numCols; j++) {
      var cell = row.cells[j];
      var cellText = cell.innerText;
      // Escape special LaTeX characters
      cellText = cellText.replace(/&/g, '\\&');
      cellText = cellText.replace(/%/g, '\\%');
      cellText = cellText.replace(/_/g, '\\_');
      cellText = cellText.replace(/#/g, '\\#');
      cellText = cellText.replace(/\\/g, '\\textbackslash{}');
      cellText = cellText.replace(/\$/g, '\\$');
      cellText = cellText.replace(/\^/g, '\\textasciicircum{}');
      latex += cellText + ' & ';
    }
    // Remove trailing '&'
    latex = latex.slice(0, -2);
    latex += ' \\\\ \n';
  }
  // Remove trailing newline
  latex = latex.slice(0, -1);

  // LaTeX table environment
  latex = '\\begin{tabular}{|' + 'c|'.repeat(numCols) + '}\n' +
    '\\hline\n' +
    latex + '\n' +
    '\\hline\n' +
    '\\end{tabular}';

  return latex;
}

function copyToClipboard(text) {
    var dummy = document.createElement("textarea");
    // to avoid breaking orgain page when copying more words
    // cant copy when adding below this code
    // dummy.style.display = 'none'
    document.body.appendChild(dummy);
    //Be careful if you use texarea. setAttribute('value', value), which works with "input" does not work with "textarea". – Eduard
    dummy.value = text;
    dummy.select();
    document.execCommand("copy");
    document.body.removeChild(dummy);
}
