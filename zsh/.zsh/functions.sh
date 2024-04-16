# docker

function backup_docker_volume() {
    if [ -z "$1" ]; then
        echo "Usage: backup_docker_volume <volume_name>"
        return 1
    fi

    local volume_name=$1
    local backup_file="${volume_name}_backup_$(date +%Y%m%d_%H%M%S).tar.gz"

    docker run --rm -v $volume_name:/$volume_name -v $(pwd):/backup ubuntu tar czvf /backup/$backup_file /$volume_name

    echo "Backup of $volume_name created as $backup_file in the current directory."
}

function restore_docker_volume() {
    if [ -z "$1" ] || [ -z "$2" ]; then
        echo "Usage: restore_docker_volume <volume_name> <backup_file>"
        return 1
    fi

    local volume_name=$1
    local backup_file=$2

    # Check if the backup file exists
    if [ ! -f "$backup_file" ]; then
        echo "Backup file $backup_file not found."
        return 1
    fi

    # Create a new volume with the same name
    docker volume create $volume_name

    # Restore the backup to the new volume
    docker run --rm -v $volume_name:/$volume_name -v $(pwd):/backup ubuntu bash -c "cd /$volume_name && tar xzvf /backup/$backup_file --strip 1"

    echo "Restored $backup_file to $volume_name."
}