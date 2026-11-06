# Privacy

cleanmodels works on your files on your own computer. Checking, repairing, compiling, decompiling and making variants send nothing anywhere: no telemetry, no update checks, no analytics.

The one exception is the bug report, which runs only when you ask for it.

## Bug reports

`cleanmodels report`, and the bug-report button in [cleanmodels-qt](https://github.com/plenarius/cleanmodels-qt), which runs it, send a report to a relay service at `cleanmodels.layonara.com`, run by the cleanmodels maintainer. The relay opens an issue in this project's public GitHub [issue tracker](https://github.com/plenarius/cleanmodels/issues) for you, so you don't need a GitHub account.

A report contains:

- the model files you name, each at most 10 MB and 25 MB in all;
- the cleanmodels version, your operating system (for example `windows`) and processor architecture (for example `amd64`);
- the command, error text and notes, if you give them with `--command`, `--error` and `--notes`, or in the GUI's report dialog.

It does not contain your name, user name, email address, or any other file or folder on your computer. The model files are sent under their file names only, without the folders they are in.

The issue, and the model files and text in the report, are public on GitHub. Don't report a model you can't share publicly, and don't put personal details in the notes.

Like any web server, the relay sees the IP address a report comes from. It uses the address only to limit how many reports can be sent, to stop abuse, and does not publish it.

## Removing a report

To have a report removed, [open an issue](https://github.com/plenarius/cleanmodels/issues) or contact the maintainer through GitHub with a link to the issue, and it will be deleted along with its files.
