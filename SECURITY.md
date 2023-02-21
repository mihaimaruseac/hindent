# Security Policy

Hindent processes source code. As such, it is normal for hindent to perform read and write operations. However, it is a vulnerability if hindent can be made to access files outside of those specified with the command invocation.

It is also a vulnerability if malformed input files cause hindent to terminate uncleanly. If an error is not surfaced to the user, and/or it is possible to observe intermediate state (that is, making the hindent invocation be not idempotent), then this could signal a code weakness that we should fix.

## Reporting a Vulnerability

For any vulnerability or code weakness, there are 2 options available:

- open an issue, if the impact of the report is bounded
- privately send me an email with details about the issue. My email is the same as the GitHub username, at Gmail.

There is a third possibility, of using private vulnerability reporting that GitHub offers, but right now I am not confident that this is properly set up. In case this works, this should be the prefered method of reporting.

In any case, for every report, I'm planning to give a first response in at most 7 days, usually in the next 24 hours. The first response will contain details about future plans to handle the vulnerability, when will it get patched, what versions will be updated, whether a CVE will be assigned, etc.

## Supported Versions

Currently, only the latest released version will be supported.

However, if a significant vulnerability is discovered and there are versions that are too incompatible with the latest one but used by a significant number of users, patch releases for these versions will be done on a best effort basis.
