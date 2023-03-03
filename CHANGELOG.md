Change Log
==========
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](https://semver.org/).

[0.4.0] - 2023-XX-XX
--------------------
* New Features
* Deprecations
* Documentation
* Bug Fix
  * Removed chained indices for improved compliance with numpy 1.24 and greater
* Maintenance
  * Adopted latest pysat development standards.
  * Adopt pytest syntax

[0.3.2] - 2022-05-13
--------------------
* New Features
  * Compatible with pysat v3.0+
* Deprecations
* Documentation
  * Added pull request templates and other GitHub project documentation.
  * Switched Windows installation instructions to favor installing WSL.
* Bug Fix
  * Improved builds for newer compilers.
  * Replaces uninterpretable characters with '*' so data loading may continue.
* Maintenance
  * Adopted latest pysat development standards.
  * Shifted from TravisCI to GitHub Actions for online testing.
  * Adopted setup.cfg
  * Improved PEP8 compliance
