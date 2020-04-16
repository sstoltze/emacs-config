#/usr/bin/fish

set -l number_of_versions_to_keep 50

# All lambdas
set -l lambda_names (aws lambda list-functions | jq '.Functions | map(.FunctionName) | .[]' | string trim -c \")

set -l total_old 0 0

for lambda_name in $lambda_names
    # Find all versions of this lambda
    set -l versions (aws lambda list-versions-by-function --function-name $lambda_name | jq '.Versions | map(select(.Version != "$LATEST") | .Version | tonumber)')

    # Stats to see how many there are
    set -l version_stats (echo $versions | jq 'min, max, length')

    # Only keep the newest 100 versions
    set -l old_versions (echo $versions | jq ".[0:-$number_of_versions_to_keep] | .[]")

    if test "$version_stats[3]" -ne 0
        echo "Stats for '$lambda_name' lambda:"
        echo "⤷Versions: $version_stats[1] - $version_stats[2]"
        echo "⤷Number of versions: $version_stats[3]"

        if test -n "$old_versions"
            echo "Deleting old versions would remove" (count $old_versions) "versions."
            set total_old (math $total_old[1] "+" (count $old_versions)) (math $total_old[2] "+ 1")
            # Uncomment the following to delete old versions
            for old in $old_versions
                echo "Deleting version $old."
                aws lambda delete-function --function-name $lambda_name --qualifier $old
            end
        end

        echo ""
    end
end

echo "Found" $total_old[1] "old versions of" $total_old[2] "lambdas."
