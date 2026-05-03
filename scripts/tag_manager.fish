#!/usr/bin/env fish

function main
    read -P "Enter tag name: " tag_name
    
    if test -z "$tag_name"
        echo "Error: Tag name required"
        exit 1
    end
    
    if not git rev-parse --git-dir >/dev/null 2>&1
        echo "Error: Not in a Git repository"
        exit 1
    end
    
    if git tag -l | grep -q "^$tag_name\$"
        echo "Tag '$tag_name' exists. Deleting..."
        git tag -d "$tag_name"
        
        if git ls-remote --tags origin | grep -q "refs/tags/$tag_name\$"
            git push origin --delete "$tag_name"
        end
    end
    
    echo "Creating tag '$tag_name'..."
    git tag "$tag_name"
    
    if test $status -ne 0
        echo "Error: Failed to create tag"
        exit 1
    end
    
    read -P "Push to remote? [Y/n]: " push_confirm
    
    if test "$push_confirm" != "n" -a "$push_confirm" != "N"
        git push origin "$tag_name"
    end
    
    echo "Done. Tag '$tag_name' created."
end

main