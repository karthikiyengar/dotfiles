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