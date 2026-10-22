# Branch protection for required PR tests

The `Tests` GitHub Actions workflow runs the Emacs test suite for pull requests. To prevent merges when those tests fail, configure branch protection or a repository ruleset to require these status checks:

- `Emacs 29.4`
- `Emacs 30.1`

## Configure in the GitHub UI

1. Open the repository on GitHub.
2. Go to **Settings** → **Branches**.
3. Under **Branch protection rules**, choose **Add branch protection rule** or edit the existing rule for your default branch.
4. Set the branch name pattern, for example `main` or `master`.
5. Enable **Require status checks to pass before merging**.
6. Search for and select both workflow checks listed above.
7. Enable **Require branches to be up to date before merging** if you want GitHub to re-run checks after the branch is updated from the base branch.
8. Save the rule.

> Note: GitHub only shows status checks after the workflow has run at least once.

## Configure with GitHub CLI

If you have repository admin permissions and `gh` installed, run:

```sh
.github/scripts/require-tests-checks.sh OWNER REPO BRANCH
```

For example:

```sh
.github/scripts/require-tests-checks.sh vv111y org-xob.el main
```

The script requires an authenticated GitHub CLI session with permission to update branch protection.
