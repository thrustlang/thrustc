#!/bin/sh

main() {
    # Read tag name from user
    printf "Enter tag name: "
    read tag_name
    
    # Check if tag name is empty
    if [ -z "$tag_name" ]; then
        echo "Error: Tag name required"
        exit 1
    fi
    
    # Check if we're in a Git repository
    if ! git rev-parse --git-dir >/dev/null 2>&1; then
        echo "Error: Not in a Git repository"
        exit 1
    fi
    
    # Check if tag already exists locally and delete it
    if git tag -l | grep -q "^$tag_name\$"; then
        echo "Tag '$tag_name' exists. Deleting..."
        git tag -d "$tag_name"
        
        # Check if tag exists on remote and delete it
        if git ls-remote --tags origin | grep -q "refs/tags/$tag_name\$"; then
            git push origin --delete "$tag_name"
        fi
    fi
    
    # Create new tag
    echo "Creating tag '$tag_name'..."
    git tag "$tag_name"
    
    # Check if tag creation was successful
    if [ $? -ne 0 ]; then
        echo "Error: Failed to create tag"
        exit 1
    fi
    
    # Ask user if they want to push to remote
    printf "Push to remote? [Y/n]: "
    read push_confirm
    
    # Push to remote unless user explicitly says no
    if [ "$push_confirm" != "n" ] && [ "$push_confirm" != "N" ]; then
        git push origin "$tag_name"
    fi
    
    echo "Done. Tag '$tag_name' created."
}

# Call main function
main