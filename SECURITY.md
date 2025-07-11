# Security Policy

## Supported Versions

Use this section to tell people about which versions of your project are
currently being supported with security updates.

| Version | Supported          |
| ------- | ------------------ |
| 5.1.x   | :white_check_mark: |
| 5.0.x   | :x:                |
| 4.0.x   | :white_check_mark: |
| < 4.0   | :x:                |

> **Note:** We follow Semantic Versioning. Only **minor** and **patch** releases
> (e.g. `5.1.0 → 5.1.1`) receive security fixes. Major version bumps (e.g.
> `5.1 → 6.0`) may introduce breaking changes and are not guaranteed to
> receive backported patches.

## Reporting a Vulnerability

If you discover a potential security vulnerability in this project, please follow these steps:

1. **Email** me at **danielshunom2@gmail.com**.  
   - Use a clear subject line, e.g. “[SECURITY] SQL Injection in v5.1.2”.
2. **Include**  
   - A detailed description of the issue  
   - Affected version(s)  
   - Proof-of-concept or reproduction steps  
   - Your suggested remediation (if known)  
3. We will acknowledge receipt within **48 hours** and provide a CVE assignment if applicable.
4. **Timeline**  
   - **T+7 days:** Initial analysis completed; estimated fix date communicated.  
   - **T+30 days:** Security patch released or public statement explaining delay.  
5. If you do **not** receive a response within 48 hours, please send a reminder to
   **security@example.com** or open an issue tagged `security` in our issue tracker.

## Security Update Releases

- Patches for security issues will be published as **patch** releases (e.g. `5.1.2 → 5.1.3`).
- We aim to backport critical and high-severity fixes to the **latest patch** of each supported minor version.
- Security releases follow the same versioning but are tagged in Git as `v5.1.3-secfix`.

## Severity Classification

We classify vulnerabilities according to [CVSS v3.1](https://www.first.org/cvss/):
- **Critical** (CVSS ≥ 9.0): immediate risk of data breach or full system compromise.
- **High** (7.0 ≤ CVSS < 9.0): significant impact, quick exploit.
- **Medium** (4.0 ≤ CVSS < 7.0): limited impact or requires complicated steps.
- **Low** (CVSS < 4.0): informational or minor impact.

## Disclosure Policy

- **Responsible disclosure**: we ask researchers to give us up to **90 days** to
  issue fixes before any public disclosure.
- If a fix is not available after 90 days, we will publish a mitigations guide
  and schedule a security release.
- We appreciate coordinated public disclosure and will work with you on a joint
  advisory if desired.


_Last updated: July 11, 2025_
