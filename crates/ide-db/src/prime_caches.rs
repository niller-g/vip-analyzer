//! rust-analyzer is lazy and doesn't compute anything unless asked. This
//! sometimes is counter productive when, for example, the first goto definition
//! request takes longer to compute. This module implements prepopulation of
//! various caches, it's not really advanced at the moment.

use hir::Symbol;
use salsa::{Cancelled, Database};

use crate::{
    FxIndexMap, RootDatabase,
    base_db::{Project, RootQueryDb},
};

/// We're indexing many projects.
#[derive(Debug)]
pub struct ParallelPrimeCachesProgress {
    /// the projects that we are currently priming.
    pub projects_currently_indexing: Vec<Symbol>,
    /// the total number of projects we want to prime.
    pub projects_total: usize,
    /// the total number of projects that have finished priming
    pub projects_done: usize,
    pub work_type: &'static str,
}

pub fn parallel_prime_caches(
    db: &RootDatabase,
    num_worker_threads: usize,
    cb: &(dyn Fn(ParallelPrimeCachesProgress) + Sync),
) {
    let _p = tracing::info_span!("parallel_prime_caches").entered();

    let projects_to_prime = db.all_projects().to_vec();

    enum ParallelPrimeCacheWorkerProgress {
        BeginProjectDefMap { project_id: Project, project_name: Symbol },
        EndProjectDefMap { project_id: Project },
        Cancelled(Cancelled),
    }

    let (def_map_work_sender, progress_receiver) = {
        let (progress_sender, progress_receiver) = crossbeam_channel::unbounded();
        let (def_map_work_sender, def_map_work_receiver) = crossbeam_channel::unbounded();
        let prime_caches_worker = move |db: RootDatabase| {
            let handle_def_map = |project_id, project_name| {
                progress_sender.send(ParallelPrimeCacheWorkerProgress::BeginProjectDefMap {
                    project_id,
                    project_name,
                })?;

                let cancelled = Cancelled::catch(|| _ = hir::project_def_map(&db, project_id));

                match cancelled {
                    Ok(()) => progress_sender
                        .send(ParallelPrimeCacheWorkerProgress::EndProjectDefMap { project_id })?,
                    Err(cancelled) => progress_sender
                        .send(ParallelPrimeCacheWorkerProgress::Cancelled(cancelled))?,
                }

                Ok::<_, crossbeam_channel::SendError<_>>(())
            };

            loop {
                db.unwind_if_revision_cancelled();

                // Biased because we want to prefer def maps.
                crossbeam_channel::select_biased! {
                    recv(def_map_work_receiver) -> work => {
                        let Ok((project_id, project_name)) = work else { break };
                        handle_def_map(project_id, project_name)?;
                    }
                }
            }
            Ok::<_, crossbeam_channel::SendError<_>>(())
        };

        for id in 0..num_worker_threads {
            stdx::thread::Builder::new(
                stdx::thread::ThreadIntent::Worker,
                format!("PrimeCaches#{id}"),
            )
            .allow_leak(true)
            .spawn({
                let worker = prime_caches_worker.clone();
                let db = db.clone();
                move || worker(db)
            })
            .expect("failed to spawn thread");
        }

        (def_map_work_sender, progress_receiver)
    };

    let project_def_maps_total = projects_to_prime.len();
    let mut project_def_maps_done = 0;

    // an index map is used to preserve ordering so we can sort the progress report in order of
    // "longest project to index" first
    let mut projects_currently_indexing =
        FxIndexMap::with_capacity_and_hasher(num_worker_threads, Default::default());

    for project in projects_to_prime {
        let project_name = project.extra_data(db).project_name.clone();
        def_map_work_sender.send((project, project_name)).ok();
    }

    while project_def_maps_done < project_def_maps_total {
        db.unwind_if_revision_cancelled();

        let progress = ParallelPrimeCachesProgress {
            projects_currently_indexing: projects_currently_indexing.values().cloned().collect(),
            projects_done: project_def_maps_done,
            projects_total: project_def_maps_total,
            work_type: "Indexing",
        };

        cb(progress);

        // Biased to prefer progress updates (and because it's faster).
        let progress = match progress_receiver.recv() {
            Ok(p) => p,
            Err(crossbeam_channel::RecvError) => {
                // all our workers have exited, mark us as finished and exit
                cb(ParallelPrimeCachesProgress {
                    projects_currently_indexing: vec![],
                    projects_done: project_def_maps_done,
                    projects_total: project_def_maps_done,
                    work_type: "Done",
                });
                return;
            }
        };

        match progress {
            ParallelPrimeCacheWorkerProgress::BeginProjectDefMap { project_id, project_name } => {
                projects_currently_indexing.insert(project_id, project_name);
            }
            ParallelPrimeCacheWorkerProgress::EndProjectDefMap { project_id } => {
                projects_currently_indexing.swap_remove(&project_id);
                project_def_maps_done += 1;
            }
            ParallelPrimeCacheWorkerProgress::Cancelled(cancelled) => {
                // Cancelled::throw should probably be public
                std::panic::resume_unwind(Box::new(cancelled));
            }
        }
    }
}
