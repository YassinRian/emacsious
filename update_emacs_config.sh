#!/bin/bash

# Exit on any error
set -e

# Configuration
REPO_PATH="$HOME/.emacs.d"
REPO_URL="git@github.com:yassinrian/emacsious.git"
BRANCH="main"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored messages
print_message() {
    local color=$1
    local message=$2
    echo -e "${color}${message}${NC}"
}

# Function to check if we're in the right directory
check_directory() {
    if [[ ! -d "$REPO_PATH" ]]; then
        print_message "$RED" "Error: $REPO_PATH does not exist"
        exit 1
    fi
    cd "$REPO_PATH"
}

# Function to check git repository status
check_git_repo() {
    if [[ ! -d .git ]]; then
        print_message "$BLUE" "Initializing git repository..."
        git init
        git remote add origin "$REPO_URL"
    fi
}

# Function to check for changes
check_changes() {
    if git diff-index --quiet HEAD --; then
        print_message "$BLUE" "No changes to commit"
        exit 0
    fi
}

# Main script
main() {
    print_message "$BLUE" "Starting Emacs configuration update..."
    
    # Check and enter the correct directory
    check_directory
    
    # Ensure git repository is set up
    check_git_repo
    
    # Check for changes
    if [[ -d .git ]]; then
        check_changes
    fi
    
    # Create commit message with timestamp
    local commit_msg="Update Emacs configuration - $(date +'%Y-%m-%d %H:%M:%S')"
    
    # Add all changes
    print_message "$BLUE" "Adding changes..."
    git add .
    
    # Commit changes
    print_message "$BLUE" "Committing changes..."
    git commit -m "$commit_msg"
    
    # Ensure we're on the main branch
    print_message "$BLUE" "Switching to $BRANCH branch..."
    git branch -M "$BRANCH"
    
    # Push changes
    print_message "$BLUE" "Pushing to remote..."
    git push -u origin "$BRANCH"
    
    print_message "$GREEN" "✔ Emacs configuration successfully updated!"
}

# Run the script
main
