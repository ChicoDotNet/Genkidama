#include <stdio.h>

typedef enum {
    NODE_FILE,
    NODE_FOLDER
} NodeKind;

typedef struct Node Node;

struct Node {
    NodeKind kind;
    int bytes;
    Node **children;
    int child_count;
};

static int node_size(const Node *node) {
    if (node->kind == NODE_FILE) {
        return node->bytes;
    }

    int total = 0;
    for (int i = 0; i < node->child_count; ++i) {
        total += node_size(node->children[i]);
    }
    return total;
}

int main(void) {
    Node readme = {NODE_FILE, 2, NULL, 0};
    Node api = {NODE_FILE, 3, NULL, 0};
    Node guide = {NODE_FILE, 5, NULL, 0};
    Node *docs_children[] = {&api, &guide};
    Node docs = {NODE_FOLDER, 0, docs_children, 2};
    Node *root_children[] = {&readme, &docs};
    Node root = {NODE_FOLDER, 0, root_children, 2};

    printf("leaf=%d\n", node_size(&readme));
    printf("docs=%d\n", node_size(&docs));
    printf("root=%d\n", node_size(&root));
    return 0;
}
