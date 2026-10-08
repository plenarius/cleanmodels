package main

import (
	"fmt"
	"os"
	"runtime"
	"runtime/pprof"
)

// init enables CPU and memory profiling when the CLEANMODELS_CPUPROFILE
// and CLEANMODELS_MEMPROFILE env vars are set. Both variables hold an
// output path. Profiling is intended for ad-hoc investigation; the
// hook stays inert unless explicitly opted in.
func init() {
	if path := os.Getenv("CLEANMODELS_CPUPROFILE"); path != "" {
		f, err := os.Create(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels: cpuprofile create: %v\n", err)
			return
		}
		if err := pprof.StartCPUProfile(f); err != nil {
			fmt.Fprintf(os.Stderr, "cleanmodels: cpuprofile start: %v\n", err)
			f.Close()
			return
		}
		profileCleanups = append(profileCleanups, func() {
			pprof.StopCPUProfile()
			f.Close()
		})
	}
	if path := os.Getenv("CLEANMODELS_MEMPROFILE"); path != "" {
		profileCleanups = append(profileCleanups, func() {
			f, err := os.Create(path)
			if err != nil {
				fmt.Fprintf(os.Stderr, "cleanmodels: memprofile create: %v\n", err)
				return
			}
			defer f.Close()
			runtime.GC()
			if err := pprof.WriteHeapProfile(f); err != nil {
				fmt.Fprintf(os.Stderr, "cleanmodels: memprofile write: %v\n", err)
			}
		})
	}
}

var profileCleanups []func()

func runProfileCleanups() {
	for _, fn := range profileCleanups {
		fn()
	}
}
