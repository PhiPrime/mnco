const links = ["https://radius.mathnasium.com/Student", 
"https://radius.mathnasium.com/CustomerAccount", 
"https://radius.mathnasium.com/ProgressReportManager/CurrentBatchDetail",
"https://radius.mathnasium.com/Enrollment/EnrollmentReport",
"https://radius.mathnasium.com/AssessmentReport"];

const scripts = [
`var gridReport = $("#gridStudentReport").data("kendoGrid");
gridReport.saveAsExcel();`,
`var gridReport = $("#gridAccountsReport").data("kendoGrid");
	gridReport.dataSource.read().then(function() {
	gridReport.saveAsExcel()
	})`,
`var gridReport = $("#gridLPReport").data("kendoGrid");
	gridReport.dataSource.read().then(function() {
		gridReport.saveAsExcel()
	})`,
`var gridReport = $("#gridEnrollmentReportExcel").data("kendoGrid");
	gridReport.bind("excelExport", function (e) {
	e.workbook.fileName = $("#EnrollmentStatusDropDown").data("kendoDropDownList").text() + " Report";
	var sheet = e.workbook.sheets[0];
	for (var i = 1; i < sheet.rows.length; i++) {
		var row = sheet.rows[i];
		for (var j = 0; j < row.cells.length; j++) {
			var cell = row.cells[j];
			if (typeof cell.value === "boolean" && cell.value === true)
				cell.value = "True";
			else if (typeof cell.value == "boolean" && cell.value === false)
				cell.value = "False";
		}
	}
})

gridReport.bind("excelExport", function (e) {
$("body").css("cursor", "default");
});
gridReport.saveAsExcel();`,

`document.getElementById("prePostDropDownList").value = NaN

	var gridReport = $("#gridReportExcel").data("kendoGrid");

	var s = $("#ReportStart").data("kendoDatePicker").value();
	var e = $("#ReportEnd").data("kendoDatePicker").value();
	var c = $("#centersDropDownList").data("kendoDropDownList") ? 	$("#centersDropDownList").data("kendoDropDownList").text() : "";
	gridReport.bind("excelExport", function (evt) {
	
		var name = \'Assessment Report\';
		if (c.length > 1 && c != "SelectCenter")
			name = name +  " for " + c
		if (s != null)
			name = name + " from " + kendo.toString(s, "d")
		if (e != null)
			name += " to " + kendo.toString(e, "d")
		evt.workbook.fileName = name
	})

	gridReport.dataSource.read().then(function (){
 		gridReport.saveAsExcel()
	})`
];

function accessData() {
	for (i = 0; i < 5; i++) {
		dataGet(links[i], scripts[i], i+1);
	}
}

function dataGet(link, cmd_string, download_no) {
	var accessFrame = document.createElement("iframe");
	accessFrame.setAttribute("src", link);
	document.body.append(accessFrame);
	function dataCheck() {
		try {
      			accessFrame.contentWindow.eval(cmd_string);
			console.log("Download " + String(download_no) + " complete");
    		} catch (e) {
			setTimeout(dataCheck, 250);
		}
	}

	dataCheck();	
}
