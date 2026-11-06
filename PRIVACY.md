# Privacy

cleanmodels works on your files on your own computer. Checking, repairing, compiling, decompiling and making variants send nothing anywhere: no telemetry, no update checks, no analytics.

The one exception is the bug report, which runs only when you ask for it.

## Bug reports

`cleanmodels report`, and the bug-report button in [cleanmodels-qt](https://github.com/plenarius/cleanmodels-qt), which runs it, send a report to a relay service at `cleanmodels.layonara.com`. The relay is run by the cleanmodels maintainer on Cloudflare Workers. It opens an issue in this project's public GitHub [issue tracker](https://github.com/plenarius/cleanmodels/issues) for you, so you don't need a GitHub account.

A report contains:

- the model files you name, each at most 10 MB and 25 MB in all;
- the cleanmodels version, your operating system (for example `windows`) and processor architecture (for example `amd64`);
- the command, error text and notes, if you give them with `--command`, `--error` and `--notes`, or in the GUI's report dialog.

It does not contain your name, user name, email address, or any other file or folder on your computer. The model files are sent under their file names only, without the folders they are in.

### What is public, and for how long

The issue shows the version, operating system, architecture, command, error text and notes, and links to the model files. Anyone can read the issue and download the files from those links. Don't report a model you can't share publicly, and don't put personal details in the notes.

- **The issue** stays on GitHub until it is deleted.
- **The model files** are stored by the relay (in Cloudflare's storage) and deleted automatically after 180 days. After that, the links in the issue stop working.

### What the relay keeps

- **Your IP address.** The relay limits each address to 3 reports an hour and 10 a day. To do that, it keeps a count of reports per IP address, which expires after an hour and a day. The address is not published, not logged, and not used for anything else.
- **A fingerprint of the report.** To recognise the same report sent twice, the relay keeps a hash of the error text and the files' names, sizes and first and last kilobyte, linked to the issue. It is deleted after 90 days and the files can't be recovered from it.

Nothing else is kept. Cloudflare, which hosts the relay, handles the request under its own [privacy policy](https://www.cloudflare.com/privacypolicy/).

## Removing a report

To have a report and its files removed sooner, [open an issue](https://github.com/plenarius/cleanmodels/issues) or contact the maintainer through GitHub with a link to the report's issue. The issue and its files will be deleted.
