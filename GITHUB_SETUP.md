# GitHub Setup Instructions

This guide will help you set up your R project on GitHub.

## Prerequisites

1. **GitHub Account**: You mentioned you have a GitHub account. If not, create one at https://github.com

2. **Git**: Git needs to be installed on your computer.

   **Check if Git is installed:**
   - Open PowerShell (or Command Prompt)
   - Run: `git --version`
   - If you see a version number (e.g., "git version 2.42.0"), Git is installed!
   - If you get an error, Git is not installed

   **Install Git on Windows:**
   1. Go to: https://git-scm.com/download/win
   2. Download the latest version (it will auto-detect 64-bit or 32-bit)
   3. Run the installer
   4. **Important**: During installation, choose these options:
      - ✅ "Git from the command line and also from 3rd-party software" (recommended)
      - ✅ "Use the OpenSSL library"
      - ✅ "Checkout Windows-style, commit Unix-style line endings" (default)
      - ✅ "Use Windows' default console window"
   5. Click "Next" through the rest (defaults are fine)
   6. After installation, **close and reopen PowerShell** for changes to take effect
   7. Verify installation: `git --version`

## Step 1: Initialize Git Repository (if not already done)

Open PowerShell or Terminal in your project directory (`Z:\VERBUND_MAC\R\Inn_Dikes`) and run:

```bash
git init
```

## Step 2: Configure Git (if first time)

Set your name and email (use the same email as your GitHub account):

```bash
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
```

## Step 3: Create a New Repository on GitHub

1. Go to https://github.com and log in
2. Click the "+" icon in the top right corner
3. Select "New repository"
4. Repository name: `Inn_Dikes` (or your preferred name)
5. Description: "Vegetation establishment analysis on Inn Dikes"
6. Choose **Public** or **Private** (your choice)
7. **DO NOT** initialize with README, .gitignore, or license (we already have these)
8. Click "Create repository"

## Step 4: Connect Local Repository to GitHub

After creating the repository, GitHub will show you commands. Use these:

```bash
# Add all files to staging
git add .

# Create first commit
git commit -m "Initial commit: Restructured R project with standardized paths"

# Add GitHub remote (replace YOUR_USERNAME with your GitHub username)
git remote add origin https://github.com/YOUR_USERNAME/Inn_Dikes.git

# Push to GitHub
git branch -M main
git push -u origin main
```

**Note**: You may be prompted for your GitHub username and password. For password, use a **Personal Access Token** (see Step 5).

## Step 5: Create Personal Access Token (for authentication)

GitHub no longer accepts passwords for Git operations. You need a Personal Access Token:

1. Go to GitHub → Settings → Developer settings → Personal access tokens → Tokens (classic)
2. Click "Generate new token (classic)"
3. Give it a name: "Inn_Dikes Project"
4. Select scopes: Check **repo** (full control of private repositories)
5. Click "Generate token"
6. **COPY THE TOKEN IMMEDIATELY** (you won't see it again!)
7. When Git asks for password, paste this token instead

## Step 6: Verify Upload

1. Refresh your GitHub repository page
2. You should see all your files uploaded
3. Check that `.gitignore` is working (you shouldn't see `.RData`, `.Rhistory`, etc.)

## Step 7: Future Updates

Whenever you make changes and want to update GitHub:

```bash
# Check what files changed
git status

# Add specific files (or use . for all)
git add .

# Commit with a descriptive message
git commit -m "Description of your changes"

# Push to GitHub
git push
```

## Troubleshooting

### If you get "remote origin already exists"
```bash
git remote remove origin
git remote add origin https://github.com/YOUR_USERNAME/Inn_Dikes.git
```

### If you want to see what will be committed
```bash
git status
```

### If you want to undo changes before committing
```bash
git restore <filename>
```

### If you want to see commit history
```bash
git log
```

## Best Practices

1. **Commit often** with clear messages
2. **Don't commit** large data files (they're in `.gitignore`)
3. **Use descriptive commit messages**: "Updated seed mixture analysis" is better than "fix"
4. **Pull before pushing** if working with others: `git pull` before `git push`

## Additional Resources

- GitHub Guides: https://guides.github.com
- Git Documentation: https://git-scm.com/doc
- RStudio Git Integration: If using RStudio, you can use the Git pane for visual Git operations

