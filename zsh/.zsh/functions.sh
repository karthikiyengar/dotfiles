# docker

function backup_docker_volume() {
    if [ -z "$1" ]; then
        echo "Usage: backup_docker_volume <volume_name>"
        return 1
    fi
    local volume_name=$1
    local backup_file="${volume_name}_backup_$(date +%Y%m%d_%H%M%S).tar.gz"
    docker run --rm -v $volume_name:/data -v $(pwd):/backup ubuntu tar czvf /backup/$backup_file /data
    echo "Backup of $volume_name created as $backup_file in the current directory."
}

function restore_docker_volume() {
    if [ -z "$1" ]; then
        echo "Usage: restore_docker_volume <backup_file_path>"
        return 1
    fi
    local backup_file_path=$1
    local filename=$(basename -- "$backup_file_path")
    local volume_name="${filename%%_backup_*}"

    if [ ! -f "$backup_file_path" ]; then
        echo "Backup file does not exist: $backup_file_path"
        return 1
    fi
    docker run --rm -v $volume_name:/data -v $(pwd):/backup ubuntu tar xzvf /backup/$backup_file_path -C /data
    echo "Restored $backup_file_path to volume $volume_name."
}
