
# LassoCMAQ

**LassoCMAQ** is an R Shiny application for simulating ozone (O₃) and PM2.5 pollution levels based on customizable emission scenarios.

Users can adjust emission factors across 17 administrative regions and 7 emission source categories. The application then generates spatial predictions of air pollution levels, visualized for four representative months: **January, April, July, and October**.

---

## Development Conventions

### Branch Strategy

* `main`: Production-ready release branch.
* `develop`: Ongoing integration branch where all features are merged.
* `feature/*`: Feature-specific branches created from `develop`.

**Guidelines**

* All pull requests must target the `develop` branch.
* The `main` branch is updated only when a version is ready for release.

### Commit Message Convention

This project follows the [Udacity Git Commit Message Style](https://udacity.github.io/git-styleguide/). Use the following prefixes to indicate the type of change:

* `feat`: Introduce a new feature
* `fix`: Fix a bug
* `docs`: Update documentation only
* `style`: Code style changes (formatting, white space, etc.)
* `refactor`: Code changes that do not fix a bug or add a feature
* `test`: Add or update tests
* `chore`: Changes to build process, dependencies, or maintenance tasks
  
