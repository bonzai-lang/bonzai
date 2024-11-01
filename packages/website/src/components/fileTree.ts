import type { CollectionEntry } from "astro:content";

export type TreeNode = {
  slug: string;
  post?: CollectionEntry<"docs">;
  children: TreeNode[];
}

// Function that creates a file tree from a list of posts, based on slicing slug
export function buildTree(data: CollectionEntry<"docs">[]): TreeNode {
  const tree: TreeNode = { slug: '/', children: [] };

  for (const post of data) {
    const parts = post.slug.split('/').filter(Boolean);
    let current = tree;

    for (const part of parts) {
      let child = current.children.find((node) => node.slug === part);

      if (!child) {
        child = { slug: part, children: [] };
        current.children.push(child);
      }

      current = child;
    }

    current.post = post;
  }

  return tree;
}