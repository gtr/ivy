pub fn did_you_mean<'a>(target: &str, candidates: impl Iterator<Item = &'a str>) -> Option<String> {
    let max_distance = (target.chars().count() / 3).max(1);
    candidates
        .map(|c| (edit_distance(target, c), c))
        .filter(|(d, _)| *d <= max_distance)
        .min_by_key(|(d, _)| *d)
        .map(|(_, c)| c.to_string())
}

fn edit_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let (n, m) = (a.len(), b.len());
    let mut d = vec![vec![0usize; m + 1]; n + 1];
    for (i, row) in d.iter_mut().enumerate() {
        row[0] = i;
    }
    for (j, cell) in d[0].iter_mut().enumerate() {
        *cell = j;
    }
    for i in 1..=n {
        for j in 1..=m {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            d[i][j] = (d[i - 1][j] + 1).min(d[i][j - 1] + 1).min(d[i - 1][j - 1] + cost);
            if i > 1 && j > 1 && a[i - 1] == b[j - 2] && a[i - 2] == b[j - 1] {
                d[i][j] = d[i][j].min(d[i - 2][j - 2] + 1);
            }
        }
    }
    d[n][m]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn suggests_close_typo() {
        let names = ["length", "map", "filter"];
        assert_eq!(did_you_mean("lenght", names.into_iter()), Some("length".to_string()));
        assert_eq!(did_you_mean("mpa", names.into_iter()), Some("map".to_string()));
    }

    #[test]
    fn no_suggestion_when_far() {
        let names = ["length", "map"];
        assert_eq!(did_you_mean("zzzzzz", names.into_iter()), None);
    }
}
